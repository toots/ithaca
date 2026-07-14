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

val put_profile : string -> Profile_t.profile -> unit
val get_profile : string -> Profile_t.profile

(* [max_entries] bounds how many entries any single hash may hold: a hash
   reaching it is dropped at store time and skipped at search time
   (0 disables the limit). *)
val operations : ?max_entries:int -> string -> Db.operations
