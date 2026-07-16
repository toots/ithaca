(* Ithaca - Audio fingerprinting
 * Copyright (C) 2026 Romain Beauxis
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 *)

let make_processor = Audio.make_fcqt

(* Spill a hash stream to a JSON file and read it back, so a producer that
   outruns the consumer can hold its backlog on disk instead of in memory.
   [write_hashes] consumes (pulls) the stream. *)
let write_hashes = Hashes.write_stream
let read_hashes = Hashes.read_stream

let hash_file ?(probes = false) ?fcqt ~merger ~params filename =
  let open_wav ?fcqt merger =
    let wav = Wav.fopen filename in
    let hashes = Audio.hash_wav ?fcqt ~merger ~params ~probes wav in
    fun () ->
      match hashes () with
      | Some value -> Some value
      | None ->
          Wav.close wav;
          None
  in
  let is_mono () =
    let wav = Wav.fopen filename in
    let mono = Wav.channels wav = 1 in
    Wav.close wav;
    mono
  in
  match (merger, fcqt) with
  | Audio.Single merger, _ -> open_wav ?fcqt merger
  | Audio.Both, _ when is_mono () -> open_wav ?fcqt Audio.mono_merger
  | Audio.Both, Some fcqt ->
      (* Reuse the single processor: hash the mono mix to completion, then the
         center mix, then merge — sequential so the processor is never shared
         across concurrent consumers. *)
      let mono = IStream.pull (open_wav ~fcqt Audio.mono_merger) in
      let center = IStream.pull (open_wav ~fcqt Audio.center_merger) in
      Hashes.merge (IStream.make mono) (IStream.make center)
  | Audio.Both, None ->
      Hashes.merge_parallel
        (open_wav Audio.mono_merger)
        (open_wav Audio.center_merger)

let db_params_of_profile profile =
  {
    Db.max_id_per_hash = profile.Profile.max_hash_id;
    max_pos_per_hash = profile.Profile.max_hash_pos;
  }

type t = {
  ops : Db.operations;
  db_params : Db.params;
  db : Db.t;
  sync : unit -> unit;
}

let open_db ?(nosync = false) ~profile ~db_params path =
  (* Open the env up front so the [nosync] flag takes effect before the first
     profile write caches the env with the default. *)
  Lmdb_store.open_env ~nosync path;
  let raw = Lmdb_store.operations path in
  (* Write the profile to the database on the first put, matching
     [Args.lmdb_operations]. *)
  let first = ref true in
  let put values =
    if !first then begin
      Lmdb_store.put_profile path profile;
      first := false
    end;
    raw.Db.put values
  in
  let ops = { raw with Db.put } in
  let sync () = Lmdb_store.sync path in
  { ops; db_params; db = Db.make db_params ops; sync }

let store t l = t.db.Db.insert l
let put_stored t stored = t.ops.Db.put stored
let sync t = t.sync ()
