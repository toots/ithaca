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

let opt_int name = function
  | None -> ""
  | Some n -> Printf.sprintf "%s %d " name n

let opt_flag name = function false -> "" | true -> Printf.sprintf "%s " name

let opt_string name = function
  | None -> ""
  | Some s -> Printf.sprintf "%s %s " name (Filename.quote s)

(* Hashing flags passed to [hash_to_json]. The profile they produce is
   serialized into the JSON output and reloaded by ithaca_json_store, so the
   store side needs no flags of its own. *)
let hash_flags ~b1_divisor ~reassign ~scheme ~quads_per_peak ~max_hash_entries =
  Printf.sprintf "%s%s%s%s%s"
    (opt_int "-b1-divisor" b1_divisor)
    (opt_flag "-reassign" reassign)
    (opt_string "-scheme" scheme)
    (opt_int "-quads-per-peak" quads_per_peak)
    (opt_int "-max-hash-entries" max_hash_entries)

(* Hash [file] into [json_path] without touching any database: [-output json]
   emits the hashes (with the embedded profile) to stdout. *)
let hash_to_json ~ithaca_bin ~b1_divisor ~reassign ~scheme ~quads_per_peak
    ~max_hash_entries ~on_stage ~json_path file id =
  let wav = Filename.temp_file "ithaca_idx" ".wav" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove wav with _ -> ())
    (fun () ->
      on_stage "converting";
      Ffmpeg.to_wav file wav
      &&
      (on_stage "hashing";
       Shell.run_cmd
         "%s -mode store -output json %s-i %s -id %d > %s 2>/dev/null"
         (Filename.quote ithaca_bin)
         (hash_flags ~b1_divisor ~reassign ~scheme ~quads_per_peak
            ~max_hash_entries)
         (Filename.quote wav) id (Filename.quote json_path)))

(* Load a batch of JSON hash files into the database serially. [ithaca_json_store]
   writes a [.touch] sibling per stored file and a [.error] sibling on failure. *)
let store_json ~json_store_bin ~db_path json_paths =
  let inputs =
    String.concat " "
      (List.map (fun p -> Printf.sprintf "-i %s" (Filename.quote p)) json_paths)
  in
  Shell.run_cmd "%s -lmdb-path %s %s > /dev/null 2>&1"
    (Filename.quote json_store_bin)
    (Filename.quote db_path) inputs

let search_wav ~ithaca_bin db_path wav =
  let raw =
    Shell.capture_cmd
      (Printf.sprintf
         "%s -mode search -lmdb-path %s -i %s -output json 2>/dev/null"
         (Filename.quote ithaca_bin)
         (Filename.quote db_path) (Filename.quote wav))
  in
  try Search_j.results_of_string (String.trim raw) with _ -> []

let random_bits rng =
  let a = Random.State.bits rng land 0xFFFF in
  let b = Random.State.bits rng land 0xFFFF in
  Printf.sprintf "%04x%04x" a b
