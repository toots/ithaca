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
type data = { id_r : uint16; pos_r : uint16; id_d : uint32; pos_d : uint32 }
type values = data list
type max_id = int
type match_entry = { id : int; pos : int }

type params = {
  max_id_per_hash : uint16;
  max_pos_per_hash : uint16;
  saturate : bool;
}

type stored_hashes = (hash * values) list

type operations = {
  put : max_id -> stored_hashes -> unit;
  get : hash list -> values list;
}

type search_match = match_entry list

type t = {
  insert : (int * Hashes.t) list -> unit;
  search : Hashes.hash list -> search_match list;
}

module Set = Set.Make (Int32)

let make { saturate; max_id_per_hash; max_pos_per_hash } { put; get } =
  let insert l =
    let hashes =
      List.fold_left
        (fun cur (id, hashes) ->
          let hashes_data = Hashtbl.create 1024 in
          let rec f hashes_set =
            match hashes () with
            | None -> hashes_set
            | Some { Hashes.pos; hash } ->
                let id_r = id mod max_id_per_hash in
                let pos_r = pos mod max_pos_per_hash in
                let id_d = id / max_id_per_hash in
                let pos_d = pos / max_pos_per_hash in
                Hashtbl.add hashes_data hash { id_r; pos_r; id_d; pos_d };
                let hashes_set = Set.add hash hashes_set in
                f hashes_set
          in
          let hashes =
            Set.fold
              (fun hash cur ->
                let data = Hashtbl.find_all hashes_data hash in
                (hash, data) :: cur)
              (f Set.empty) []
          in
          hashes @ cur)
        [] l
    in
    let max_id = if saturate then max_id_per_hash else 0 in
    put max_id hashes
  in

  let search hashes =
    let rec unpack_key_values cur = function
      | [] -> cur
      | { id_r; pos_r; id_d; pos_d } :: rem ->
          unpack_key_values
            ({
               id = (id_d * max_id_per_hash) + id_r;
               pos = (pos_d * max_pos_per_hash) + pos_r;
             }
            :: cur)
            rem
    in
    List.map (unpack_key_values []) (get hashes)
  in
  { insert; search }
