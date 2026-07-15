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

open OUnit2

let suite =
  [
    ( "hashes spill round-trip" >:: fun _ ->
      let entries =
        [
          { Hashes.pos = 0; hash = 42; bin = 3 };
          (* Above 2^53: exercises jsont's string encoding of large ints. *)
          { Hashes.pos = 123456; hash = max_int; bin = 0 };
          { Hashes.pos = 7; hash = (1 lsl 60) + 12345; bin = 127 };
        ]
      in
      let path = Filename.temp_file "ithaca_test_hashes" ".json" in
      Fun.protect
        ~finally:(fun () -> try Sys.remove path with _ -> ())
        (fun () ->
          Store.write_hashes path (IStream.make entries);
          assert_equal entries (IStream.pull (Store.read_hashes path))) );
    ( "hashes spill empty" >:: fun _ ->
      let path = Filename.temp_file "ithaca_test_hashes" ".json" in
      Fun.protect
        ~finally:(fun () -> try Sys.remove path with _ -> ())
        (fun () ->
          Store.write_hashes path (IStream.make []);
          assert_equal [] (IStream.pull (Store.read_hashes path))) );
  ]
