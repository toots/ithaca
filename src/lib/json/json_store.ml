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

open Db

let unpack =
  List.map (fun (hash, values) ->
      ( hash,
        List.map
          (fun { Stored_hashes_t.id_r; pos_r; id_d; pos_d } ->
            { Db.id_r; pos_r; id_d; pos_d })
          values ))

let put _ hashes =
  let hashes =
    {
      Stored_hashes_t.profile = Args.base64_profile ();
      hashes =
        List.map
          (fun (hash, values) ->
            ( hash,
              List.map
                (fun { id_r; pos_r; id_d; pos_d } ->
                  { Stored_hashes_t.id_r; pos_r; id_d; pos_d })
                values ))
          hashes;
    }
  in
  let buf = Buffer.create 4096 in
  Stored_hashes_j.write_stored_hashes buf hashes;
  print_string (Buffer.contents buf)

let operations = { Db.put; get = (fun _ -> failwith "Not implemented!") }
