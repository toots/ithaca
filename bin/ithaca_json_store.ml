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

let usage = "json_store -i <json file> [-i <json file>] [-d <directory>] <args>"
let input_filenames = ref []
let add_input filename = input_filenames := filename :: !input_filenames

let add_directory dirname =
  let n = String.length dirname in
  let dirname =
    if dirname.[n - 1] = '/' then String.sub dirname 0 (n - 1) else dirname
  in
  input_filenames :=
    !input_filenames
    @ Array.fold_left
        (fun cur fname ->
          if Filename.extension fname = ".json" then
            Printf.sprintf "%s/%s" dirname fname :: cur
          else cur)
        [] (Sys.readdir dirname)

let batch_size = ref 10

let args =
  [
    ("-i", Arg.String add_input, "Input file");
    ("-d", Arg.String add_directory, "Input directory");
    ("-b", Arg.Int (fun b -> batch_size := b), "Processing batch size");
  ]
  @ [ Args.store_arg ]

let time f =
  let start_time = Unix.gettimeofday () in
  let ret = f () in
  let processing_time = Unix.gettimeofday () -. start_time in
  (ret, processing_time)

let print_time t =
  let t = int_of_float t in
  Printf.sprintf "%02d:%02d:%02d" (t / 3600) (t / 60 mod 60) (t mod 60)

let () =
  Args.parse ~args usage;
  if !input_filenames = [] then begin
    Printf.eprintf "%s\n%!" usage;
    exit 1
  end;
  let (), time =
    time (fun () ->
        Consolidate.load
          ~profile_of_string:(fun s ->
            Args.set_base64_profile s;
            Args.get_profile ())
          ~batch_size:!batch_size (Args.get_lmdb_path ()) !input_filenames)
  in
  Printf.printf "Processing time: %s\n%!" (print_time time)
