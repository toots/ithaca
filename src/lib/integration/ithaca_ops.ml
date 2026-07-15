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

let search_wav ~ithaca_bin db_path wav =
  let raw =
    Shell.capture_cmd
      (Printf.sprintf
         "%s -mode search -lmdb-path %s -i %s -output json 2>/dev/null"
         (Filename.quote ithaca_bin)
         (Filename.quote db_path) (Filename.quote wav))
  in
  try Search.of_string (String.trim raw) with _ -> []

let random_bits rng =
  let a = Random.State.bits rng land 0xFFFF in
  let b = Random.State.bits rng land 0xFFFF in
  Printf.sprintf "%04x%04x" a b
