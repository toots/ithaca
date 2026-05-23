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

type 'a t = unit -> 'a option

let make l =
  let s = Stream.of_list l in
  fun () -> try Some (Stream.next s) with Stream.Failure -> None

let pull s =
  let ret = ref [] in
  let rec pull () =
    match s () with
    | Some x ->
        ret := x :: !ret;
        pull ()
    | None -> ()
  in
  pull ();
  List.rev !ret
