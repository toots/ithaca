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

exception Error of int
exception Inconsistent_profile

external string_of_error : int -> string = "ocaml_lmdb_string_of_error"

let () =
  Callback.register_exception "ocaml_lmdb_error" (Error 0);
  Printexc.register_printer (function
    | Error code -> Some (string_of_error code)
    | _ -> None)

type t
type values = Db.data array

external connect : string -> t = "ocaml_lmdb_open"
external close : t -> unit = "ocaml_lmdb_close"
external lmdb_put_profile : t -> string -> unit = "ocaml_lmdb_put_profile"
external lmdb_get_profile : t -> string = "ocaml_lmdb_get_profile"

external lmdb_put : t -> int -> (int32 * values) array -> unit
  = "ocaml_lmdb_put"

let wrap ?(must_exist = true) path f arg =
  if (not (Sys.file_exists path)) && must_exist then
    failwith (Printf.sprintf "Ithaca LMDB database does not exist: %s" path);
  let env = connect path in
  try
    let ret = f env arg in
    close env;
    ret
  with e ->
    close env;
    raise e

let put_profile path =
  wrap ~must_exist:false path (fun env profile ->
      let profile = Profile_b.string_of_profile profile in
      begin try
        if profile <> lmdb_get_profile env then raise Inconsistent_profile
      with Not_found -> ()
      end;
      lmdb_put_profile env profile)

let get_profile path =
  wrap path
    (fun env () ->
      let profile = lmdb_get_profile env in
      Profile_b.profile_of_string profile)
    ()

let put path max =
  wrap ~must_exist:false path (fun env hashes ->
      let hashes =
        Array.of_list
          (List.map (fun (hash, values) -> (hash, Array.of_list values)) hashes)
      in
      lmdb_put env max hashes)

external lmdb_get : t -> int32 array -> values array = "ocaml_lmdb_get"

let get path =
  wrap path (fun env hashes ->
      List.map Array.to_list
        (Array.to_list (lmdb_get env (Array.of_list hashes))))

let operations path = { Db.put = put path; get = get path }
