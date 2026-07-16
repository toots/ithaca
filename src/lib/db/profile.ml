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
  samplerate : int;
  frame_step : float;
  min_freq : float;
  max_freq : float;
  bins_per_octave : float;
  reassign : bool;
  scheme : string;
  quads_per_peak : int;
  delta_x : float;
  delta_y : int;
  max_x : float;
  max_y : int;
  merger : string;
  max_hash_id : int;
  max_hash_pos : int;
  search_frame_length : float;
  search_frame_step : float;
  search_buffer_size : int;
  search_threshold : int;
  whitening_time : float;
}

let jsont =
  Jsont.Object.map
    (fun
      samplerate
      frame_step
      min_freq
      max_freq
      bins_per_octave
      reassign
      scheme
      quads_per_peak
      delta_x
      delta_y
      max_x
      max_y
      merger
      max_hash_id
      max_hash_pos
      search_frame_length
      search_frame_step
      search_buffer_size
      search_threshold
      whitening_time
    ->
      {
        samplerate;
        frame_step;
        min_freq;
        max_freq;
        bins_per_octave;
        reassign;
        scheme;
        quads_per_peak;
        delta_x;
        delta_y;
        max_x;
        max_y;
        merger;
        max_hash_id;
        max_hash_pos;
        search_frame_length;
        search_frame_step;
        search_buffer_size;
        search_threshold;
        whitening_time;
      })
  |> Jsont.Object.mem "samplerate" Jsont.int ~enc:(fun p -> p.samplerate)
  |> Jsont.Object.mem "frame_step" Jsont.number ~enc:(fun p -> p.frame_step)
  |> Jsont.Object.mem "min_freq" Jsont.number ~enc:(fun p -> p.min_freq)
  |> Jsont.Object.mem "max_freq" Jsont.number ~enc:(fun p -> p.max_freq)
  |> Jsont.Object.mem "bins_per_octave" Jsont.number ~enc:(fun p ->
      p.bins_per_octave)
  |> Jsont.Object.mem "reassign" Jsont.bool ~enc:(fun p -> p.reassign)
  |> Jsont.Object.mem "scheme" Jsont.string ~dec_absent:"pairs" ~enc:(fun p ->
      p.scheme)
  |> Jsont.Object.mem "quads_per_peak" Jsont.int ~dec_absent:6 ~enc:(fun p ->
      p.quads_per_peak)
  |> Jsont.Object.mem "delta_x" Jsont.number ~enc:(fun p -> p.delta_x)
  |> Jsont.Object.mem "delta_y" Jsont.int ~enc:(fun p -> p.delta_y)
  |> Jsont.Object.mem "max_x" Jsont.number ~enc:(fun p -> p.max_x)
  |> Jsont.Object.mem "max_y" Jsont.int ~enc:(fun p -> p.max_y)
  |> Jsont.Object.mem "merger" Jsont.string ~enc:(fun p -> p.merger)
  |> Jsont.Object.mem "max_hash_id" Jsont.int ~enc:(fun p -> p.max_hash_id)
  |> Jsont.Object.mem "max_hash_pos" Jsont.int ~enc:(fun p -> p.max_hash_pos)
  |> Jsont.Object.mem "search_frame_length" Jsont.number ~dec_absent:0.
       ~enc:(fun p -> p.search_frame_length)
  |> Jsont.Object.mem "search_frame_step" Jsont.number ~dec_absent:0.
       ~enc:(fun p -> p.search_frame_step)
  |> Jsont.Object.mem "search_buffer_size" Jsont.int ~dec_absent:0
       ~enc:(fun p -> p.search_buffer_size)
  |> Jsont.Object.mem "search_threshold" Jsont.int ~dec_absent:0 ~enc:(fun p ->
      p.search_threshold)
  |> Jsont.Object.mem "whitening_time" Jsont.number ~enc:(fun p ->
      p.whitening_time)
  |> Jsont.Object.finish

let of_string s =
  match Jsont_bytesrw.decode_string jsont s with
  | Ok v -> v
  | Error msg -> failwith msg

let to_string p =
  match Jsont_bytesrw.encode_string jsont p with
  | Ok s -> s
  | Error msg -> failwith msg
