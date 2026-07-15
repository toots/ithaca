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

let get_duration file =
  let s =
    Shell.capture_cmd
      (Printf.sprintf
         "ffprobe -v quiet -show_entries format=duration -of \
          default=noprint_wrappers=1:nokey=1 %s 2>/dev/null"
         (Filename.quote file))
  in
  try float_of_string (String.trim s) with _ -> 0.0

let to_wav input output =
  Shell.run_cmd "ffmpeg -nostdin -y -i %s -ar 44100 -ac 2 -f wav %s 2>/dev/null"
    (Filename.quote input) (Filename.quote output)

let extract_clip input output start dur =
  Shell.run_cmd "ffmpeg -nostdin -y -i %s -ss %g -t %g -f wav %s 2>/dev/null"
    (Filename.quote input) start dur (Filename.quote output)

let pitch_shift input output semitones =
  let ratio = 2.0 ** (semitones /. 12.0) in
  Shell.run_cmd
    "ffmpeg -nostdin -y -i %s -af rubberband=pitch=%g -f wav %s 2>/dev/null"
    (Filename.quote input) ratio (Filename.quote output)

let mix_sfx ?(offset = 0.0) ?(mono = false) ?(source_lufs = -14.0)
    ?(sfx_lufs = -10.0) input sfx output =
  let sfx_chain =
    if mono then
      Printf.sprintf "aformat=channel_layouts=mono,loudnorm=I=%g" sfx_lufs
    else Printf.sprintf "loudnorm=I=%g" sfx_lufs
  in
  Shell.run_cmd
    "ffmpeg -nostdin -y -i %s -ss %g -stream_loop -1 -i %s -filter_complex \
     '[0:a]loudnorm=I=%g[a];[1:a]%s[s];[a][s]amix=inputs=2:duration=first' -ar \
     44100 -acodec pcm_s16le -f wav %s 2>/dev/null"
    (Filename.quote input) offset (Filename.quote sfx) source_lufs sfx_chain
    (Filename.quote output)

let audio_extensions =
  [ ".mp3"; ".flac"; ".wav"; ".ogg"; ".m4a"; ".aac"; ".opus"; ".wv" ]

let is_audio file =
  List.mem (String.lowercase_ascii (Filename.extension file)) audio_extensions

let find_audio_files dir =
  let acc = ref [] in
  let rec walk path =
    match Sys.readdir path with
    | exception _ -> ()
    | entries ->
        Array.iter
          (fun entry ->
            let full = Filename.concat path entry in
            if Sys.is_directory full then walk full
            else if is_audio entry then acc := full :: !acc)
          entries
  in
  walk dir;
  List.sort String.compare !acc
