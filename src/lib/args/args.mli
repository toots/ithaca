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

type arg = Arg.key * Arg.spec * Arg.doc

val quads_max_hash_entries : int
(** Default [-max-hash-entries] value applied to the quads scheme. *)

val parse :
  ?allow_anon:bool ->
  args:(Arg.key * Arg.spec * Arg.doc) list ->
  Arg.usage_msg ->
  unit

val anonymous_args : unit -> string list
val profile_arg : arg
val b1_divisor_arg : arg
val reassign_arg : arg
val scheme_arg : arg
val quads_per_peak_arg : arg
val max_hash_entries_arg : arg
val whitening_time_arg : arg

(* Programmatic equivalents of the CLI flags above, for callers that configure
   the hashing profile without parsing an argv (e.g. the integration indexer).
   Same side effects as the corresponding [*_arg] actions. *)
val set_b1_divisor : int -> unit
val set_reassign : unit -> unit
val set_scheme : string -> unit
val set_quads_per_peak : int -> unit
val set_max_hash_entries : int -> unit
val base64_profile : unit -> string
val set_base64_profile : string -> unit
val json_profile : unit -> string
val get_profile : unit -> Profile.t
val audio_params : unit -> Audio.audio_params
val merger : unit -> Audio.merger_mode
val store_arg : arg
val get_lmdb_path : unit -> string
val lmdb_operations : unit -> Db.operations
val db_params : unit -> Db.params
val search_params : unit -> Search.search_params
