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

(* Shared hash-and-store operations, extracted from [bin/ithaca.ml]'s [store]
   and reused by the binary and the integration indexer. *)

val make_processor : Audio.audio_params -> Fcqt.t
(** Build a reusable CQT processor for [params]. A caller hashing many files
    across domains builds one per worker up front (serially, before spawning the
    workers) and passes it to [hash_file] as [~fcqt], keeping the
    non-thread-safe FFTW planning off the worker domains. *)

val hash_file :
  ?probes:bool ->
  ?fcqt:Fcqt.t ->
  merger:Audio.merger_mode ->
  params:Audio.audio_params ->
  string ->
  Hashes.t
(** Hash a WAV file. When [~fcqt] is given it is reused (and, for the [Both]
    merger, the two channel mixes are hashed sequentially on the caller's domain
    so the shared processor is never used concurrently). Without [~fcqt], [Both]
    falls back to [Hashes.merge_parallel] as before. *)

type t
(** A database open for the lifetime of the value: the underlying LMDB
    environment stays open across every [store]/[put_stored] call. *)

val db_params_of_profile : Profile_t.profile -> Db.params
val open_db : profile:Profile_t.profile -> db_params:Db.params -> string -> t

val store : t -> (int * Hashes.t) list -> unit
(** Store raw hash streams (a list is one write transaction). *)

val put_stored : t -> Db.stored_hashes -> unit
(** Store already-packed hashes (e.g. decoded from JSON), applying the profile's
    saturation cap. *)
