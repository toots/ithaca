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

type peak = int * int
type hash = int
type hashes = { pos : int; hash : hash }
type t = hashes IStream.t

module HashSet : Set.S with type elt = hash

(* Returns a stream of frames of [length] samples taken each [step] samples. *)
val frames :
  length:int -> step:int -> float array IStream.t -> float array IStream.t

(* Returns a stream of points who are local maximun in a square of [delta_x, delta_y] size around them. *)
val peaks :
  delta_x:int -> delta_y:int -> float array IStream.t -> peak list IStream.t

(* Returns a stream of pair of peaks who are close by at most [max_x] on [x] and [max_y] on [y]. *)
val pairs :
  delta_x:int ->
  delta_y:int ->
  max_x:int ->
  max_y:int ->
  peak list IStream.t ->
  (peak * peak) list IStream.t

(* Hash function: encodes b̂1=⌊y1/b1_divisor⌋, Δbin, and Δtime. *)
val hash : ?b1_divisor:int -> peak -> peak -> hash
val hashes : ?b1_divisor:int -> (peak * peak) list IStream.t -> t

(* Merge two hash streams into one, interleaved by position. *)
val merge : t -> t -> t

(* Same as [merge], but each stream is consumed to completion in its own
   domain first. Both streams must already be fully constructed: FFTW plan
   creation is not thread-safe, only stream consumption is. *)
val merge_parallel : t -> t -> t
