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

let read_file filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let touch filename =
  let oc = open_out filename in
  close_out oc

let rec batches n = function
  | [] -> []
  | l ->
      let rec take k acc = function
        | rest when k = 0 -> (List.rev acc, rest)
        | x :: rest -> take (k - 1) (x :: acc) rest
        | [] -> (List.rev acc, [])
      in
      let chunk, rest = take n [] l in
      chunk :: batches n rest

let load ~profile_of_string ~batch_size db_path filenames =
  (* One database handle for the whole run, opened lazily from the first
     successfully-parsed profile (all files share it). *)
  let db = ref None in
  let db_for profile =
    match !db with
    | Some d -> d
    | None ->
        let d =
          Store.open_db ~profile
            ~db_params:(Store.db_params_of_profile profile)
            db_path
        in
        db := Some d;
        d
  in
  List.iter
    (fun batch ->
      let marks, hashes, profile =
        List.fold_left
          (fun (marks, hashes, profile) filename ->
            let base = Filename.remove_extension filename in
            let tname = base ^ ".touch" in
            let errname = base ^ ".error" in
            if Sys.file_exists tname then begin
              Printf.eprintf "Skipping already indexed file %s\n%!" filename;
              (marks, hashes, profile)
            end
            else if Sys.file_exists errname then begin
              Printf.eprintf "Skipping erroneous %s\n%!" filename;
              (marks, hashes, profile)
            end
            else begin
              Printf.printf "Storing JSON data from file %s..\n%!" filename;
              try
                let data =
                  Stored_hashes_j.stored_hashes_of_string (read_file filename)
                in
                let profile = profile_of_string data.Stored_hashes_j.profile in
                ( (fun () -> touch tname) :: marks,
                  hashes @ Json_store.unpack data.Stored_hashes_j.hashes,
                  Some profile )
              with e ->
                Printf.eprintf "Error while parsing %s: %s\n" filename
                  (Printexc.to_string e);
                ((fun () -> touch errname) :: marks, hashes, profile)
            end)
          ([], [], None) batch
      in
      (match profile with
      | None -> ()
      | Some profile -> Store.put_stored (db_for profile) hashes);
      List.iter (fun f -> f ()) marks)
    (batches batch_size filenames)
