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

exception Inconsistent_profile
exception Error of int

val put_profile : string -> Profile.t -> unit
val get_profile : string -> Profile.t

(* Open (and cache) the environment for [path] up front. [nosync] (defering
   fsync to an explicit [sync]) is fixed at open time, so a caller that wants
   it must call this before any other operation opens the env. *)
val open_env : ?nosync:bool -> string -> unit

(* Flush the environment to disk. Only meaningful for an env opened with
   [~nosync:true], which otherwise never fsyncs. *)
val sync : string -> unit
val operations : string -> Db.operations
