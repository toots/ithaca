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

let () = Printexc.record_backtrace true

type arg = Arg.key * Arg.spec * Arg.doc

type profile = Profile_t.profile = {
  samplerate : int;
  frame_step : float;
  min_freq : float;
  max_freq : float;
  bins_per_octave : float;
  reassign : bool;
  delta_x : float;
  delta_y : int;
  max_x : float;
  max_y : int;
  merger : string;
  max_hash_id : int;
  max_hash_pos : int;
  saturate : bool;
  search_frame_length : float;
  search_frame_step : float;
  search_buffer_size : int;
  search_threshold : int;
  whitening_time : float;
}

let merger_of_string = function
  | "mono" -> Audio.Single Audio.mono_merger
  | "center" -> Audio.Single Audio.center_merger
  | "both" -> Audio.Both
  | _ -> failwith "Invalid merger!"

let profile =
  ref
    {
      Profile_t.samplerate = Audio.default_params.Audio.samplerate;
      frame_step = Audio.default_params.Audio.frame_step;
      min_freq = Audio.default_params.Audio.hashes_min_freq;
      max_freq = Audio.default_params.Audio.hashes_max_freq;
      bins_per_octave = Audio.default_params.Audio.hashes_bins_per_octave;
      whitening_time = Audio.default_params.Audio.hashes_whitening_time;
      reassign = Audio.default_params.Audio.hashes_reassign;
      delta_x = Audio.default_params.Audio.peaks_delta_x;
      delta_y = Audio.default_params.Audio.peaks_delta_y;
      max_x = Audio.default_params.Audio.pairs_max_x;
      max_y = Audio.default_params.Audio.pairs_max_y;
      merger = "both";
      max_hash_id = 512;
      max_hash_pos = 25;
      saturate = true;
      search_frame_length = Search.default_params.Search.frame_length;
      search_frame_step = Search.default_params.Search.frame_step;
      search_buffer_size = Search.default_params.Search.buffer_size;
      search_threshold = Search.default_params.Search.threshold;
    }

(* Set Default values for search params when needed. This can go away
 * once a new production DB and hash json has been generated. *)
let set_profile p =
  let f ~d ~z v = if v = z then d else v in
  profile :=
    {
      p with
      Profile_t.search_frame_length =
        f ~d:Search.default_params.Search.frame_length ~z:0.
          p.Profile_t.search_frame_length;
      search_frame_step =
        f ~d:Search.default_params.Search.frame_step ~z:0.
          p.Profile_t.search_frame_step;
      search_buffer_size =
        f ~d:Search.default_params.Search.buffer_size ~z:0
          p.Profile_t.search_buffer_size;
      search_threshold =
        f ~d:Search.default_params.Search.threshold ~z:0
          p.Profile_t.search_threshold;
    }

let default_profile = ref true

let set_json_profile arg =
  default_profile := false;
  let json =
    if Sys.file_exists arg then begin
      let ch = open_in arg in
      let json = really_input_string ch (in_channel_length ch) in
      close_in ch;
      json
    end
    else arg
  in
  set_profile (Profile_j.profile_of_string json)

let base64_profile () =
  Cryptokit.transform_string
    (Cryptokit.Base64.encode_compact ())
    (Profile_b.string_of_profile !profile)

let set_base64_profile data =
  default_profile := false;
  set_profile
    (Profile_b.profile_of_string
       (Cryptokit.transform_string (Cryptokit.Base64.decode ()) data))

let b1_divisor = ref Audio.default_params.Audio.hashes_b1_divisor

let b1_divisor_arg =
  ( "-b1-divisor",
    Arg.Int (fun n -> b1_divisor := n),
    Printf.sprintf
      "Divisor for b̂₁ = ⌊b1/N⌋ in the hash (default: %d). Larger values give \
       coarser hash granularity: more pitch-shift tolerance per variant but \
       also more hash collisions across songs."
      !b1_divisor )

let reassign_arg =
  ( "-reassign",
    Arg.Unit (fun () -> profile := { !profile with Profile_t.reassign = true }),
    "Enable frequency reassignment for sharper peak positions (slower, \
     disabled by default)." )

let whitening_time_arg =
  ( "-whitening-time",
    Arg.Float
      (fun t -> profile := { !profile with Profile_t.whitening_time = t }),
    Printf.sprintf
      "Spectral whitening EMA time constant in seconds (default: %.1f, 0 to \
       disable)."
      Audio.default_params.Audio.hashes_whitening_time )

let lmdb_path = ref "./ithaca.db"

let store_arg =
  ( "-lmdb-path",
    Arg.String (fun s -> lmdb_path := s),
    Printf.sprintf
      "Path to an existing ithaca database. The hashing profile stored in the \
       database is loaded automatically so that the same parameters are used \
       as during indexing. Default: %s"
      !lmdb_path )

let fetch_lmdb_profile () =
  if Sys.file_exists !lmdb_path then begin
    Printf.eprintf "Reading hashing profile from %s..\n%!" !lmdb_path;
    try
      let lmdb_profile = Lmdb_store.get_profile !lmdb_path in
      if !default_profile then set_profile lmdb_profile
      else if !profile <> lmdb_profile then
        raise Lmdb_store.Inconsistent_profile
    with Not_found -> ()
  end

let anonymous_args = ref []

let parse ?(allow_anon = false) ~args usage =
  let anon_fun s =
    if allow_anon then anonymous_args := s :: !anonymous_args
    else raise (Arg.Bad ("Bad argument : " ^ s))
  in
  Arg.parse args anon_fun usage;
  fetch_lmdb_profile ()

let anonymous_args () = List.rev !anonymous_args
let json_profile () = Profile_j.string_of_profile !profile

let profile_arg =
  ( "-profile",
    Arg.String set_json_profile,
    "Load hashing parameters from a JSON profile (file path or inline JSON). \
     Use this to visualize peaks with exactly the same settings as a specific \
     indexing run, without needing the full database." )

let audio_params () =
  {
    Audio.samplerate = !profile.samplerate;
    frame_step = !profile.frame_step;
    hashes_min_freq = !profile.min_freq;
    hashes_max_freq = !profile.max_freq;
    hashes_bins_per_octave = !profile.bins_per_octave;
    hashes_reassign = !profile.reassign;
    peaks_delta_x = !profile.delta_x;
    peaks_delta_y = !profile.delta_y;
    pairs_max_x = !profile.max_x;
    pairs_max_y = !profile.max_y;
    hashes_b1_divisor = !b1_divisor;
    hashes_whitening_time = !profile.whitening_time;
  }

let merger () : Audio.merger_mode = merger_of_string !profile.merger

let lmdb_operations () =
  let ops = Lmdb_store.operations !lmdb_path in
  let first = ref true in
  let put max values =
    if !first then begin
      Lmdb_store.put_profile !lmdb_path !profile;
      first := false
    end;
    ops.Db.put max values
  in
  { ops with Db.put }

let db_params () =
  {
    Db.max_id_per_hash = !profile.max_hash_id;
    max_pos_per_hash = !profile.max_hash_pos;
    saturate = !profile.saturate;
  }

let search_params () =
  {
    Search.frame_length = !profile.search_frame_length;
    frame_step = !profile.search_frame_step;
    buffer_size = !profile.search_buffer_size;
    threshold = !profile.search_threshold;
    debug = false;
  }
