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

type raw_search = Db.match_entry list
type query_entry = { rel_pos : int }

type frame = {
  ofs : int;
  hashes : Hashes.HashSet.t;
  positions : (Hashes.hash, query_entry) Hashtbl.t;
}

type result = { id : int; offset : int; delta : int; count : int }
type t

val init : (Hashes.hash list -> raw_search list) -> t
val search : t -> frame -> result option
