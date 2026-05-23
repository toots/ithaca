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

let path db_path = db_path ^ ".manifest"

let write db_path entries =
  let oc = open_out (path db_path) in
  List.iter (fun (f, id) -> Printf.fprintf oc "%d\t%s\n" id f) entries;
  close_out oc

let read db_path =
  let ic = open_in (path db_path) in
  let acc = ref [] in
  (try
     while true do
       let line = input_line ic in
       match String.split_on_char '\t' line with
       | [ id_s; p ] -> acc := (p, int_of_string id_s) :: !acc
       | _ -> ()
     done
   with End_of_file -> ());
  close_in ic;
  List.rev !acc
