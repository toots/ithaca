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

type header = {
  channels : int; (* 1 = mono ; 2 = stereo *)
  sample_rate : int; (* in Hz *)
  bytes_per_second : int;
  bytes_per_sample : int; (* 1=8 bit Mono, 2=8 bit Stereo *)
  (* or 16 bit Mono, 4=16 bit Stereo *)
  bits_per_sample : int;
  format_code : int;
}

val header_jsont : header Jsont.t

type t

val header : t -> header
val data_offset : t -> int
val from_raw : header:header -> length:int -> in_channel -> t

exception Not_a_wav_file of string

val fopen : string -> t
val in_chan_open : in_channel -> t
val samples : t -> int -> float array array
val info : t -> string
val channels : t -> int
val sample_rate : t -> int
val sample_size : t -> int
val data_length : t -> int
val duration_length : t -> float -> int
val length_duration : t -> int -> float
val duration : t -> float
val close : t -> unit
