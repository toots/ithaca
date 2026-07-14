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

(* Speed relative to realtime: audio_s / wall_s seconds of audio processed per
   wall second. Rendered as e.g. "1.2x realtime". *)
let realtime ~audio_s ~wall_s =
  if wall_s <= 0. || audio_s <= 0. then "–"
  else Printf.sprintf "%.1fx realtime" (audio_s /. wall_s)

let human_bytes b =
  let units = [| "B"; "KB"; "MB"; "GB"; "TB" |] in
  let rec go v i =
    if v >= 1024. && i < Array.length units - 1 then go (v /. 1024.) (i + 1)
    else Printf.sprintf "%.1f %s" v units.(i)
  in
  go (float b) 0

let bytes_per_second bytes audio_s =
  if audio_s <= 0. then "–"
  else human_bytes (int_of_float (float bytes /. audio_s)) ^ "/audio-s"

(* Actual on-disk size. The LMDB file is sparse (its apparent size is the 1 TiB
   map size), so Unix.stat would lie — ask du for the allocated blocks instead.
   ponytail: shells out to du; fine at once-per-file cadence. *)
let disk_bytes path =
  let out =
    String.trim
      (Shell.capture_cmd
         (Printf.sprintf "du -k %s 2>/dev/null" (Filename.quote path)))
  in
  let n = String.length out in
  let i = ref 0 in
  while !i < n && out.[!i] >= '0' && out.[!i] <= '9' do
    incr i
  done;
  match int_of_string_opt (String.sub out 0 !i) with
  | Some kb -> kb * 1024
  | None -> 0
