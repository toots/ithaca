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
    Db.max_id_per_hash = profile.Profile_t.max_hash_id;
    max_pos_per_hash = profile.Profile_t.max_hash_pos;
    saturate = profile.Profile_t.saturate;
  }

type t = { ops : Db.operations; db_params : Db.params; db : Db.t }

let open_db ~profile ~db_params path =
  let raw =
    Lmdb_store.operations ~max_entries:profile.Profile_t.max_hash_entries path
  in
  (* Write the profile to the database on the first put, matching
     [Args.lmdb_operations]. *)
  let first = ref true in
  let put max values =
    if !first then begin
      Lmdb_store.put_profile path profile;
      first := false
    end;
    raw.Db.put max values
  in
  let ops = { raw with Db.put } in
  { ops; db_params; db = Db.make db_params ops }

let store t l = t.db.Db.insert l

let put_stored t stored =
  let { Db.saturate; max_id_per_hash; _ } = t.db_params in
  let max_id = if saturate then max_id_per_hash else 0 in
  t.ops.Db.put max_id stored
