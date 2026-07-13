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

type search_params = {
  frame_length : float;
  frame_step : float;
  buffer_size : int;
  threshold : int;
  debug : bool;
}

type result = Search_t.result = {
  start : float;
  stop : float;
  id : string;
  pitch_semitones : float;
}

type search_entry = Db.match_entry list
type search = Hashes.hash list -> search_entry list

type search_match = {
  match_start : int;
  match_stop : int;
  match_id : int;
  match_offset : int;
  match_delta : int;
  match_votes : int;
  match_bin_delta : float;
}

val default_params : search_params

val frames :
  params:search_params ->
  audio_params:Audio.audio_params ->
  Hashes.t ->
  Search_map.frame IStream.t

val best_match :
  debug:bool -> Search_map.t -> Search_map.frame -> search_match option

val buffered_match :
  params:search_params ->
  search_match option Ringbuffer.t ->
  search_match option

val consolidate : result list -> result list

val search_hashes :
  ?params:search_params ->
  search:search ->
  audio_params:Audio.audio_params ->
  Hashes.t ->
  result list
