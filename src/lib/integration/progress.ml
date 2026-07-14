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

(*
   Layout (jobs = 3):
     [header]        ← top of block, jobs+1 lines above cursor
       job 0: ...    ← jobs lines above cursor
       job 1: ...
       job 2: ...    ← 1 line above cursor
                     ← cursor sits here
*)

let shorten n s =
  if String.length s > n then String.sub s 0 (n - 3) ^ "..." else s

type t = { jobs : int; header_lines : int; mutex : Mutex.t }

let create ?(header_lines = 1) jobs =
  for _ = 1 to header_lines + jobs do
    Printf.eprintf "\n%!"
  done;
  { jobs; header_lines; mutex = Mutex.create () }

(* [msg] may contain newlines; each line fills one reserved header row. *)
let update_header t msg =
  let lines = Array.of_list (String.split_on_char '\n' msg) in
  Mutex.lock t.mutex;
  Printf.eprintf "\027[%dA" (t.header_lines + t.jobs);
  for i = 0 to t.header_lines - 1 do
    let line = if i < Array.length lines then lines.(i) else "" in
    Printf.eprintf "\r\027[2K  %s" line;
    if i < t.header_lines - 1 then Printf.eprintf "\n"
  done;
  Printf.eprintf "\027[%dB\r%!" (t.jobs + 1);
  Mutex.unlock t.mutex

(* Erase the whole reserved block and leave the cursor at its top row, so
   subsequent output starts clean. *)
let clear t =
  let total = t.header_lines + t.jobs in
  Mutex.lock t.mutex;
  Printf.eprintf "\027[%dA" total;
  for i = 0 to total do
    Printf.eprintf "\r\027[2K";
    if i < total then Printf.eprintf "\027[1B"
  done;
  Printf.eprintf "\027[%dA\r%!" total;
  Mutex.unlock t.mutex

let update_job t domain_idx msg =
  let rows_up = t.jobs - domain_idx in
  Mutex.lock t.mutex;
  Printf.eprintf "\027[%dA\r\027[2K    job %d: %s\027[%dB%!" rows_up domain_idx
    msg rows_up;
  Mutex.unlock t.mutex
