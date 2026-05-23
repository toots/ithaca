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

type channel_merger = float array array -> float array

val mono_merger : channel_merger
val center_merger : channel_merger

type audio_params = {
  samplerate : int;
  frame_step : float;
  hashes_min_freq : float;
  hashes_max_freq : float;
  hashes_bins_per_octave : float;
  hashes_reassign : bool;
  peaks_delta_x : float;
  peaks_delta_y : int;
  pairs_max_x : float;
  pairs_max_y : int;
  hashes_b1_divisor : int;
  hashes_whitening_time : float;
}

val default_params : audio_params

(* This for debug, i.e. plot *)
type 'a instrument = ('a -> unit) option

type instruments = {
  cqt : float array instrument;
  peaks : Hashes.peak list instrument;
  pairs : (Hashes.peak * Hashes.peak) list instrument;
}

val hash_wav :
  ?instruments:instruments ->
  ?merger:channel_merger ->
  ?params:audio_params ->
  Wav.t ->
  Hashes.t
