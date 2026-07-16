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

(* A hashing/search profile: the parameters that must match between an
   indexing run and a search for results to be meaningful. Stored alongside
   an LMDB database and embeddable in a JSON blob (e.g. the [-profile] CLI
   flag) so a query can be run with exactly the settings a database was
   built with. *)
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

val jsont : t Jsont.t
val of_string : string -> t
val to_string : t -> string
