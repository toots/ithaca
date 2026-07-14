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
external lmdb_put_profile : t -> string -> unit = "ocaml_lmdb_put_profile"
external lmdb_get_profile : t -> string = "ocaml_lmdb_get_profile"

external lmdb_put : t -> int -> int -> (int * values) array -> unit
  = "ocaml_lmdb_put"

external lmdb_get : t -> int array -> int -> values array = "ocaml_lmdb_get"

(* Environments are opened once per path and kept for the lifetime of the
   process: the custom block finalizer closes them. Opening the same LMDB
   file twice within one process is unsafe, hence the shared table. *)
let envs : (string, t) Hashtbl.t = Hashtbl.create 4
let envs_mutex = Mutex.create ()

let env_for ?(must_exist = true) path =
  Mutex.protect envs_mutex (fun () ->
      match Hashtbl.find_opt envs path with
      | Some env -> env
      | None ->
          if must_exist && not (Sys.file_exists path) then
            failwith
              (Printf.sprintf "Ithaca LMDB database does not exist: %s" path);
          let env = connect path in
          Hashtbl.add envs path env;
          env)

let put_profile path profile =
  let env = env_for ~must_exist:false path in
  let profile = Profile_b.string_of_profile profile in
  begin try if profile <> lmdb_get_profile env then raise Inconsistent_profile
  with Not_found -> ()
  end;
  lmdb_put_profile env profile

let get_profile path =
  Profile_b.profile_of_string (lmdb_get_profile (env_for path))

let put ~max_entries path max hashes =
  let hashes =
    Array.of_list
      (List.map (fun (hash, values) -> (hash, Array.of_list values)) hashes)
  in
  lmdb_put (env_for ~must_exist:false path) max max_entries hashes

let get ~max_entries path hashes =
  List.map Array.to_list
    (Array.to_list (lmdb_get (env_for path) (Array.of_list hashes) max_entries))

(* [max_entries] bounds how many entries any single hash may hold: a hash
   reaching it is dropped and marked dead (0 disables the limit). *)
let operations ?(max_entries = 0) path =
  { Db.put = put ~max_entries path; get = get ~max_entries path }
