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

(* We suppose that machine is at least 32 bits. *)
type uint16 = int
type uint32 = int
type hash = Hashes.hash

type data = {
  id_r : uint16;
  pos_r : uint16;
  id_d : uint32;
  pos_d : uint32;
  bin : uint16;
}

type values = data list
type match_entry = { id : int; pos : int; bin : int }
type params = { max_id_per_hash : uint16; max_pos_per_hash : uint16 }
type stored_hashes = (hash * values) list

type operations = {
  put : stored_hashes -> unit;
  get : hash list -> values list;
}

type search_match = match_entry list

type t = {
  insert : (int * Hashes.t) list -> unit;
  search : Hashes.hash list -> search_match list;
}

val make : params -> operations -> t
