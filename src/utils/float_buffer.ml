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

type t = {
  queue : float array Queue.t;
  mutable cur : float array;
  mutable cur_offset : int;
  mutable size : int;
}

let init () = { queue = Queue.create (); cur = [||]; cur_offset = 0; size = 0 }
let length t = t.size + Array.length t.cur - t.cur_offset

let add t data =
  Queue.add data t.queue;
  t.size <- t.size + Array.length data

exception Done

let peek t len =
  let len = min len (length t) in
  let ret = Array.make len 0. in
  let cur_available = Array.length t.cur - t.cur_offset in
  let cur_to_copy = min cur_available len in
  let position =
    if 0 < cur_to_copy then begin
      Array.blit t.cur t.cur_offset ret 0 cur_to_copy;
      cur_to_copy
    end
    else 0
  in
  if position < len then begin
    let fill (position, remaining) data =
      let to_copy = min remaining (Array.length data) in
      Array.blit data 0 ret position to_copy;
      if remaining = to_copy then raise Done
      else (position + to_copy, remaining - to_copy)
    in
    try
      ignore (Queue.fold fill (position, len - cur_to_copy) t.queue);
      assert false
    with Done -> ()
  end;
  ret

let drop t len =
  let len = min len (length t) in
  let rec drop remaining data offset =
    let available = Array.length data - offset in
    if remaining <= available then begin
      t.cur <- data;
      t.cur_offset <- offset + remaining
    end
    else begin
      let data = Queue.take t.queue in
      t.size <- t.size - Array.length data;
      drop (remaining - available) data 0
    end
  in
  drop len t.cur t.cur_offset;
  len
