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

let index_file ~ithaca_bin ~b1_divisor ~reassign ~scheme ~on_stage db_path
    file id =
  let wav = Filename.temp_file "ithaca_idx" ".wav" in
  let opt_int name = function
    | None -> ""
    | Some n -> Printf.sprintf "%s %d " name n
  in
  let opt_flag name = function
    | false -> ""
    | true -> Printf.sprintf "%s " name
  in
  let opt_string name = function
    | None -> ""
    | Some s -> Printf.sprintf "%s %s " name (Filename.quote s)
  in
  Fun.protect
    ~finally:(fun () -> try Sys.remove wav with _ -> ())
    (fun () ->
      on_stage "converting";
      Ffmpeg.to_wav file wav
      &&
      (on_stage "hashing";
       Shell.run_cmd
         "%s -mode store %s%s%s-lmdb-path %s -i %s -id %d 2>/dev/null"
         (Filename.quote ithaca_bin)
         (opt_int "-b1-divisor" b1_divisor)
         (opt_flag "-reassign" reassign)
         (opt_string "-scheme" scheme)
         (Filename.quote db_path) (Filename.quote wav) id))

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
