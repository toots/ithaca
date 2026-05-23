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

type 'a t = { content : 'a array; mutable offset : int; size : int }

let init values =
  let size = Array.length values in
  { content = values; offset = 0; size }

let get buf pos =
  let actual_pos =
    if pos < buf.size - buf.offset then pos + buf.offset
    else pos + buf.offset - buf.size
  in
  buf.content.(actual_pos)

let push buf elem =
  buf.content.(buf.offset) <- elem;
  buf.offset <- (buf.offset + 1) mod buf.size
