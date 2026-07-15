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

type wav_channel = { input : in_channel; mutable position : int }

type header = {
  channels : int; (* 1 = mono ; 2 = stereo *)
  sample_rate : int; (* in Hz *)
  bytes_per_second : int;
  bytes_per_sample : int; (* 1=8 bit Mono, 2=8 bit Stereo *)
  (* or 16 bit Mono, 4=16 bit Stereo *)
  bits_per_sample : int;
  format_code : int;
}

let header_jsont =
  Jsont.Object.map
    (fun channels sample_rate bytes_per_second bytes_per_sample bits_per_sample
         format_code ->
      {
        channels;
        sample_rate;
        bytes_per_second;
        bytes_per_sample;
        bits_per_sample;
        format_code;
      })
    ~kind:"header"
  |> Jsont.Object.mem "channels" Jsont.int ~enc:(fun h -> h.channels)
  |> Jsont.Object.mem "sample_rate" Jsont.int ~enc:(fun h -> h.sample_rate)
  |> Jsont.Object.mem "bytes_per_second" Jsont.int ~enc:(fun h ->
      h.bytes_per_second)
  |> Jsont.Object.mem "bytes_per_sample" Jsont.int ~enc:(fun h ->
      h.bytes_per_sample)
  |> Jsont.Object.mem "bits_per_sample" Jsont.int ~enc:(fun h ->
      h.bits_per_sample)
  |> Jsont.Object.mem "format_code" Jsont.int ~enc:(fun h -> h.format_code)
  |> Jsont.Object.finish

type t = { ic : wav_channel; header : header; data_offset : int; length : int }

let header { header } = header
let data_offset { data_offset } = data_offset

let from_raw ~header ~length ic =
  let ic = { input = ic; position = 0 } in
  { ic; header; data_offset = 0; length }

exception Not_a_wav_file of string

let input_byte ch =
  let ret = input_byte ch.input in
  ch.position <- ch.position + 1;
  ret

let really_input ch bytes ofs len =
  really_input ch.input bytes ofs len;
  ch.position <- ch.position + len

let input ch bytes ofs len =
  let ret = input ch.input bytes ofs len in
  ch.position <- ch.position + ret;
  ret

(* open file and verify it has the right format *)

let in_chan_open ic =
  let ic = { input = ic; position = 0 } in
  let read_int_num_bytes ic =
    let rec aux = function
      | 0 -> 0
      | n ->
          let b = input_byte ic in
          b + (256 * aux (n - 1))
    in
    aux
  in
  let read_int ic = read_int_num_bytes ic 4 in
  let read_short ic = read_int_num_bytes ic 2 in
  let read_string ic n =
    let ans = Bytes.create n in
    really_input ic ans 0 n;
    Bytes.to_string ans
  in

  if read_string ic 4 <> "RIFF" then
    raise (Not_a_wav_file "Bad header: \"RIFF\" expected");
  ignore (read_int ic);
  (* size of the file *)
  if read_string ic 4 <> "WAVE" then
    raise (Not_a_wav_file "Bad header: \"WAVE\" expected");
  let chunk = ref (read_string ic 4) in
  while !chunk <> "fmt " do
    let len = read_int ic in
    ignore (read_string ic len);
    chunk := read_string ic 4
  done;

  let fmt_len = read_int ic in
  if fmt_len < 0x10 then
    raise (Not_a_wav_file "Bad header: invalid \"fmt \" length");
  if read_short ic <> 1 then
    raise (Not_a_wav_file "Bad header: unhandled codec");

  let chan_num = read_short ic in
  let samp_hz = read_int ic in
  let byt_per_sec = read_int ic in
  let byt_per_samp = read_short ic in
  let bit_per_samp = read_short ic in
  (* The fmt header can be padded *)
  if fmt_len > 0x10 then ignore (read_int_num_bytes ic (fmt_len - 0x10));

  let header = ref (read_string ic 4) in
  (* Skip unhandled chunks. *)
  while !header <> "data" do
    let len = read_int ic in
    ignore (read_string ic len);
    header := read_string ic 4
  done;

  let header =
    {
      channels = chan_num;
      sample_rate = samp_hz;
      bytes_per_second = byt_per_sec;
      bytes_per_sample = byt_per_samp;
      bits_per_sample = bit_per_samp;
      format_code = 1;
    }
  in

  let len_dat = read_int ic in
  { ic; header; length = len_dat; data_offset = ic.position }

let fopen file =
  let ic = open_in_bin file in
  try in_chan_open ic with
  | End_of_file ->
      close_in ic;
      raise (Not_a_wav_file "End of file unexpected")
  | e ->
      close_in ic;
      raise e

external pcm_of_u8 : string -> int -> float array array -> int -> int -> unit
  = "caml_float_pcm_of_u8_byte" "caml_float_pcm_of_u8_native"

external pcm_of_s16le :
  string -> int -> float array array -> int -> int -> bool -> unit
  = "caml_float_pcm_convert_s16le_byte" "caml_float_pcm_convert_s16le_native"

let pcm_of_s24le tmp buf samples =
  let channels = Array.length buf in
  let bytes_per_frame = 3 * channels in
  for i = 0 to samples - 1 do
    for ch = 0 to channels - 1 do
      let ofs = (i * bytes_per_frame) + (ch * 3) in
      let b0 = Char.code tmp.[ofs] in
      let b1 = Char.code tmp.[ofs + 1] in
      let b2 = Char.code tmp.[ofs + 2] in
      let v = b0 lor (b1 lsl 8) lor (b2 lsl 16) in
      let v = if v >= 0x800000 then v - 0x1000000 else v in
      buf.(ch).(i) <- float v /. 8388608.0
    done
  done

let samples w len =
  let slen = len * w.header.bytes_per_sample in
  let tmp = Bytes.make slen ' ' in
  let n =
    try
      really_input w.ic tmp 0 slen;
      slen
    with End_of_file -> input w.ic tmp 0 slen
  in
  let tmp = Bytes.to_string tmp in
  let samples = n / w.header.bytes_per_sample in
  let buf = Array.init w.header.channels (fun _ -> Array.make samples 0.) in
  begin match w.header.bits_per_sample with
  | 16 -> pcm_of_s16le tmp 0 buf 0 samples (not Sys.big_endian)
  | 24 -> pcm_of_s24le tmp buf samples
  | 8 -> pcm_of_u8 tmp 0 buf 0 samples
  | b -> raise (Not_a_wav_file (Printf.sprintf "Unsupported bit depth: %d" b))
  end;
  buf

let duration_length w d =
  int_of_float (ceil (d *. float w.header.bytes_per_second))

let length_duration w d = float d /. float w.header.bytes_per_second
let duration w = length_duration w w.length

let info ({ header } as w) =
  let duration = int_of_float (duration w) in
  Printf.sprintf "WAVE PCM Data %dch, %dHz %dbit, duration: %02d:%02d:%02d"
    header.channels header.sample_rate header.bits_per_sample (duration / 3600)
    (duration / 60 mod 60)
    (duration mod 60)

let channels { header } = header.channels
let sample_rate { header } = header.sample_rate
let sample_size { header } = header.bits_per_sample
let data_length w = w.length
let close w = close_in w.ic.input

let data_len file =
  let stats = Unix.stat file in
  stats.Unix.st_size - 36

let short_string i =
  let up = i / 256 in
  let down = i - (256 * up) in
  String.make 1 (char_of_int down) ^ String.make 1 (char_of_int up)

let int_string n =
  let b = Bytes.create 4 in
  Bytes.set b 0 (char_of_int (n land 0xff));
  Bytes.set b 1 (char_of_int ((n land 0xff00) lsr 8));
  Bytes.set b 2 (char_of_int ((n land 0xff0000) lsr 16));
  Bytes.set b 3 (char_of_int ((n land 0x7f000000) lsr 24));
  Bytes.to_string b
