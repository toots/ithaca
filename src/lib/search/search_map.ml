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

type t = {
  mutable hash_cache : Hashes.HashSet.t;
  mutable searches : (Hashes.hash, raw_search) Hashtbl.t;
  generator : Hashes.hash list -> raw_search list;
}

let init generator =
  {
    generator;
    hash_cache = Hashes.HashSet.empty;
    searches = Hashtbl.create ~random:true 1024;
  }

let search t frame =
  let to_add =
    Hashes.HashSet.elements (Hashes.HashSet.diff frame.hashes t.hash_cache)
  in

  let from_cache =
    Hashes.HashSet.elements (Hashes.HashSet.inter frame.hashes t.hash_cache)
  in

  let add_values = t.generator to_add in

  let cached_values = List.map (Hashtbl.find t.searches) from_cache in

  let hashes = from_cache @ to_add in
  let values = cached_values @ add_values in

  let counts = Hashtbl.create ~random:true 1024 in

  let result = ref None in

  let incr_entry (id, delta) offset =
    let count, offset =
      try Hashtbl.find counts (id, delta) with Not_found -> (0, offset)
    in
    Hashtbl.replace counts (id, delta) (count + 1, offset);
    match !result with
    | None -> result := Some { id; delta; count; offset }
    | Some r when r.count <= count ->
        result := Some { id; delta; count; offset }
    | _ -> ()
  in

  List.iter2
    (fun hash entries ->
      List.iter
        (fun { Db.id; pos = track_ofs } ->
          let query_entries = Hashtbl.find_all frame.positions hash in
          List.iter
            (fun { rel_pos } ->
              let absolute_offset = frame.ofs + rel_pos - track_ofs in
              incr_entry (id, absolute_offset) track_ofs)
            query_entries)
        entries)
    hashes values;

  t.searches <- Hashtbl.create ~random:true 1024;

  List.iter2 (Hashtbl.replace t.searches) hashes values;

  t.hash_cache <- frame.hashes;

  !result
