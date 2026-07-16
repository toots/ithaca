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

type stored_hashes_file = { profile : string; hashes : Db.stored_hashes }

let data_jsont =
  Jsont.Object.map
    (fun id_r pos_r id_d pos_d bin : Db.data ->
      { id_r; pos_r; id_d; pos_d; bin })
    ~kind:"data"
  |> Jsont.Object.mem "id_r" Jsont.int ~enc:(fun (d : Db.data) -> d.id_r)
  |> Jsont.Object.mem "pos_r" Jsont.int ~enc:(fun (d : Db.data) -> d.pos_r)
  |> Jsont.Object.mem "id_d" Jsont.int ~enc:(fun (d : Db.data) -> d.id_d)
  |> Jsont.Object.mem "pos_d" Jsont.int ~enc:(fun (d : Db.data) -> d.pos_d)
  |> Jsont.Object.mem "bin" Jsont.int ~enc:(fun (d : Db.data) -> d.bin)
  |> Jsont.Object.finish

let entry_jsont =
  Jsont.Object.map (fun hash values -> (hash, values)) ~kind:"hash_values"
  |> Jsont.Object.mem "hash" Jsont.int ~enc:fst
  |> Jsont.Object.mem "values" (Jsont.list data_jsont) ~enc:snd
  |> Jsont.Object.finish

let stored_hashes_jsont : Db.stored_hashes Jsont.t = Jsont.list entry_jsont

let file_jsont =
  Jsont.Object.map
    (fun profile hashes -> { profile; hashes })
    ~kind:"stored_hashes_file"
  |> Jsont.Object.mem "profile" Jsont.string ~enc:(fun f -> f.profile)
  |> Jsont.Object.mem "hashes" stored_hashes_jsont ~enc:(fun f -> f.hashes)
  |> Jsont.Object.finish

let of_string s =
  match Jsont_bytesrw.decode_string file_jsont s with
  | Ok v -> v
  | Error msg -> failwith msg

let to_string file =
  match Jsont_bytesrw.encode_string file_jsont file with
  | Ok s -> s
  | Error msg -> failwith msg

let put hashes =
  print_string (to_string { profile = Args.base64_profile (); hashes })

let operations = { Db.put; get = (fun _ -> failwith "Not implemented!") }
