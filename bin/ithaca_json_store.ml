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

let get_firsts length l =
  let rec pull chunk = function
    | tl when List.length chunk == length -> (chunk, tl)
    | el :: tl -> pull (chunk @ [ el ]) tl
    | [] -> (chunk, [])
  in
  pull [] l

let split_list length l =
  let rec split cur = function
    | [] -> cur
    | rem ->
        let next, rem = get_firsts length rem in
        split (cur @ [ next ]) rem
  in
  split [] l

let () =
  Args.parse ~args usage;
  if !input_filenames = [] then begin
    Printf.eprintf "%s\n%!" usage;
    exit 1
  end;
  let (), time =
    time (fun () ->
        List.iter
          (fun filenames ->
            let touch, hashes =
              List.fold_left
                (fun (touch, cur) filename ->
                  let tname =
                    Printf.sprintf "%s.touch"
                      (Filename.remove_extension filename)
                  in
                  let errname =
                    Printf.sprintf "%s.error"
                      (Filename.remove_extension filename)
                  in
                  if Sys.file_exists tname then begin
                    Printf.eprintf "Skipping already indexed file %s\n%!"
                      filename;
                    (touch, cur)
                  end
                  else if Sys.file_exists errname then begin
                    Printf.eprintf "Skipping erroneous %s\n%!" filename;
                    (touch, cur)
                  end
                  else begin
                    Printf.printf "Storing JSON data from file %s..\n%!"
                      filename;
                    let json_string =
                      let ic = open_in filename in
                      let n = in_channel_length ic in
                      let s = Bytes.create n in
                      really_input ic s 0 n;
                      close_in ic;
                      Bytes.to_string s
                    in
                    let tname, hashes =
                      try
                        let data =
                          Stored_hashes_j.stored_hashes_of_string json_string
                        in
                        (* Load profile *)
                        Args.set_base64_profile data.Stored_hashes_j.profile;
                        let hashes =
                          Json_store.unpack data.Stored_hashes_j.hashes
                        in
                        (tname, hashes)
                      with e ->
                        Printf.eprintf "Error while parsing %s: %s\n" filename
                          (Printexc.to_string e);
                        (errname, [])
                    in
                    (* Touch file *)
                    let f () =
                      let oc = open_out tname in
                      close_out oc
                    in
                    (f :: touch, cur @ hashes)
                  end)
                ([], []) filenames
            in
            let operations = Args.lmdb_operations () in
            let { Db.saturate; max_id_per_hash } = Args.db_params () in
            let max_id = if saturate then max_id_per_hash else 0 in
            operations.Db.put max_id hashes;
            List.iter (fun f -> f ()) touch)
          (split_list !batch_size !input_filenames))
  in
  Printf.printf "Processing time: %s\n%!" (print_time time)
