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

type params = {
  min_freq : float;
  max_freq : float;
  bins_per_octave : float;
  samplerate : float;
  step : float;
  reassign : bool;
}

type frame = Complex.t array
type t

exception Need_more_data

val init : params -> t

val reset : t -> unit
(** Clear per-file transient state so the processor (plans + kernels) can be
    reused for another file. *)

val sample_size : t -> int

val execute_frame : t -> float array -> frame
(** Forward CQT: time-domain frame → complex CQT coefficients. *)

val frame_magnitude : frame -> float array
(** Extract power spectrum from a complex frame (discards phase). *)

val invert_frame : t -> frame -> float array
(** Adjoint CQT: complex CQT coefficients → approximate time-domain frame. *)

val execute : t -> float array -> float array
(** [execute] is [execute_frame] followed by [frame_magnitude], with optional
    reassignment. Use [execute_frame] / [invert_frame] for the invertible path.
*)
