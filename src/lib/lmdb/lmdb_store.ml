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

external connect : string -> bool -> t = "ocaml_lmdb_open"
external lmdb_sync : t -> unit = "ocaml_lmdb_sync"
external lmdb_put_profile : t -> string -> unit = "ocaml_lmdb_put_profile"
external lmdb_get_profile : t -> string = "ocaml_lmdb_get_profile"
external lmdb_put : t -> (int * values) array -> unit = "ocaml_lmdb_put"
external lmdb_get : t -> int array -> values array = "ocaml_lmdb_get"

(* Environments are opened once per path and kept for the lifetime of the
   process: the custom block finalizer closes them. Opening the same LMDB
   file twice within one process is unsafe, hence the shared table. *)
let envs : (string, t) Hashtbl.t = Hashtbl.create 4
let envs_mutex = Mutex.create ()

let env_for ?(must_exist = true) ?(nosync = false) path =
  Mutex.protect envs_mutex (fun () ->
      match Hashtbl.find_opt envs path with
      | Some env -> env
      | None ->
          if must_exist && not (Sys.file_exists path) then
            failwith
              (Printf.sprintf "Ithaca LMDB database does not exist: %s" path);
          let env = connect path nosync in
          Hashtbl.add envs path env;
          env)

(* Open (and cache) the environment up front. The [nosync] flag is fixed at
   open time, so a caller that wants it must force the open before any other
   operation caches the env with the default flag. *)
let open_env ?(nosync = false) path =
  ignore (env_for ~must_exist:false ~nosync path)

let sync path = lmdb_sync (env_for path)

let put_profile path profile =
  let env = env_for ~must_exist:false path in
  let profile = Profile.to_string profile in
  begin try if profile <> lmdb_get_profile env then raise Inconsistent_profile
  with Not_found -> ()
  end;
  lmdb_put_profile env profile

let get_profile path = Profile.of_string (lmdb_get_profile (env_for path))

let put path hashes =
  let hashes =
    Array.of_list
      (List.map (fun (hash, values) -> (hash, Array.of_list values)) hashes)
  in
  lmdb_put (env_for ~must_exist:false path) hashes

let get path hashes =
  List.map Array.to_list
    (Array.to_list (lmdb_get (env_for path) (Array.of_list hashes)))

let operations path = { Db.put = put path; get = get path }
