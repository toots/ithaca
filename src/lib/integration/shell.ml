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

let run_cmd fmt =
  Printf.ksprintf
    (fun cmd ->
      let pid =
        Unix.create_process "/bin/sh" [| "/bin/sh"; "-c"; cmd |] Unix.stdin
          Unix.stdout Unix.stderr
      in
      match Unix.waitpid [] pid with _, Unix.WEXITED 0 -> true | _ -> false)
    fmt

let capture_cmd cmd =
  let ic = Unix.open_process_in cmd in
  let buf = Buffer.create 256 in
  (try
     while true do
       Buffer.add_channel buf ic 1
     done
   with End_of_file -> ());
  ignore (Unix.close_process_in ic);
  Buffer.contents buf
