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

(* Quad-based hashing (Sonnleitner & Widmer, IEEE TASLP 2016): four peaks
   normalized into the unit box spanned by the two outer ones, quantized to
   a 32-bit hash. Invariant to pitch shifting on the log-frequency axis. *)

(* Hash of a single quad: anchor, far corner, and two interior peaks. *)
val hash : Hashes.peak -> Hashes.peak -> Hashes.peak -> Hashes.peak -> int

(* Turn a peak stream into a quad hash stream. With [probes] (query side),
   components near a quantization-cell boundary also emit the adjacent
   cell's hash to tolerate boundary jitter; without (index side), exactly
   one hash per quad. *)
val hashes :
  ?probes:bool ->
  max_x:int ->
  max_y:int ->
  Hashes.peak list IStream.t ->
  Hashes.t
