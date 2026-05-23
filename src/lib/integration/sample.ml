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

let expand_home path =
  if String.length path > 1 && path.[0] = '~' && path.[1] = '/' then
    let home = try Sys.getenv "HOME" with Not_found -> "" in
    home ^ String.sub path 1 (String.length path - 1)
  else path

let load_sfx sfx_dir =
  if sfx_dir = "" then [||]
  else begin
    let sfx_dir = expand_home sfx_dir in
    if (not (Sys.file_exists sfx_dir)) || not (Sys.is_directory sfx_dir) then begin
      Printf.eprintf
        "Error: sfx-dir '%s' does not exist or is not a directory\n%!" sfx_dir;
      exit 1
    end;
    let files = Array.of_list (Ffmpeg.find_audio_files sfx_dir) in
    if Array.length files = 0 then begin
      Printf.eprintf "Error: sfx-dir '%s' contains no audio files\n%!" sfx_dir;
      exit 1
    end;
    files
  end

let shuffle rng arr =
  for i = Array.length arr - 1 downto 1 do
    let j = Random.State.int rng (i + 1) in
    let t = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- t
  done

let setup_workdir ?(prefix = "ithaca_") samples_dir =
  let samples_dir = expand_home samples_dir in
  if samples_dir <> "" then begin
    if not (Sys.file_exists samples_dir) then Unix.mkdir samples_dir 0o755;
    samples_dir
  end
  else begin
    let d = Filename.temp_file prefix "" in
    Sys.remove d;
    Unix.mkdir d 0o755;
    at_exit (fun () ->
        ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote d))));
    d
  end
