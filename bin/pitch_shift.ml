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

let () =
  if Array.length Sys.argv < 4 then begin
    Printf.eprintf "Usage: %s input.wav output.wav semitones\n" Sys.argv.(0);
    exit 1
  end;
  let input_file = Sys.argv.(1) in
  let output_file = Sys.argv.(2) in
  let semitones = float_of_string Sys.argv.(3) in
  let pitch_ratio = 2.0 ** (semitones /. 12.0) in

  let wav = Wav.fopen input_file in
  let channels = Wav.channels wav in
  let samplerate = Wav.sample_rate wav in

  let st = Soundtouch.make channels samplerate in
  Soundtouch.set_pitch st pitch_ratio;

  (* Feed all input samples *)
  let rec feed () =
    let chunk = Wav.samples wav 4096 in
    if Array.length chunk.(0) > 0 then begin
      Soundtouch.put_samples_ni st chunk 0 (Array.length chunk.(0));
      feed ()
    end
  in
  feed ();
  Soundtouch.flush st;
  Wav.close wav;

  (* Collect all output samples *)
  let buf = Array.init channels (fun _ -> Buffer.create 65536) in
  let out_buf = Array.init channels (fun _ -> Array.make 4096 0.) in
  let rec collect () =
    let n = Soundtouch.get_samples_ni st out_buf 0 4096 in
    if n > 0 then begin
      for c = 0 to channels - 1 do
        for i = 0 to n - 1 do
          (* 16-bit little-endian PCM *)
          let s = int_of_float (Float.round (out_buf.(c).(i) *. 32767.)) in
          let s = max (-32768) (min 32767 s) in
          Buffer.add_uint8 buf.(c) (s land 0xFF);
          Buffer.add_uint8 buf.(c) ((s lsr 8) land 0xFF)
        done
      done;
      collect ()
    end
  in
  collect ();

  (* Interleave channels and write WAV *)
  let n_samples = Buffer.length buf.(0) / 2 in
  let data_size = n_samples * channels * 2 in
  let oc = open_out output_file in
  (* RIFF header *)
  output_string oc "RIFF";
  let write_i32 v =
    output_char oc (Char.chr (v land 0xFF));
    output_char oc (Char.chr ((v lsr 8) land 0xFF));
    output_char oc (Char.chr ((v lsr 16) land 0xFF));
    output_char oc (Char.chr ((v lsr 24) land 0xFF))
  in
  let write_i16 v =
    output_char oc (Char.chr (v land 0xFF));
    output_char oc (Char.chr ((v lsr 8) land 0xFF))
  in
  write_i32 (36 + data_size);
  output_string oc "WAVEfmt ";
  write_i32 16;
  (* chunk size *)
  write_i16 1;
  (* PCM *)
  write_i16 channels;
  write_i32 samplerate;
  write_i32 (samplerate * channels * 2);
  (* byte rate *)
  write_i16 (channels * 2);
  (* block align *)
  write_i16 16;
  (* bits per sample *)
  output_string oc "data";
  write_i32 data_size;
  (* Interleave channels *)
  let bufs = Array.map (fun b -> Bytes.of_string (Buffer.contents b)) buf in
  for i = 0 to n_samples - 1 do
    for c = 0 to channels - 1 do
      output_char oc (Bytes.get bufs.(c) (i * 2));
      output_char oc (Bytes.get bufs.(c) ((i * 2) + 1))
    done
  done;
  close_out oc
