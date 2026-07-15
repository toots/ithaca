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

val load :
  profile_of_string:(string -> Profile.t) ->
  batch_size:int ->
  string ->
  string list ->
  unit
(** [load ~profile_of_string ~batch_size db_path files] loads JSON hash files
    into the database at [db_path] through a single open handle. Files whose
    [.touch]/[.error] sibling already exists are skipped; a [.touch] is written
    per stored file, a [.error] on parse failure. [profile_of_string] decodes
    the profile embedded in each JSON. *)
