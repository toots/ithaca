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

type result = {
  start : float;
  stop : float;
  id : string;
  pitch_semitones : float;
}

let result_jsont =
  Jsont.Object.map
    (fun start stop id pitch_semitones -> { start; stop; id; pitch_semitones })
    ~kind:"result"
  |> Jsont.Object.mem "start" Jsont.number ~enc:(fun r -> r.start)
  |> Jsont.Object.mem "stop" Jsont.number ~enc:(fun r -> r.stop)
  |> Jsont.Object.mem "id" Jsont.string ~enc:(fun r -> r.id)
  |> Jsont.Object.mem "pitch_semitones" Jsont.number ~dec_absent:0.
       ~enc:(fun r -> r.pitch_semitones)
  |> Jsont.Object.finish

let results_jsont = Jsont.list result_jsont

let of_string s =
  match Jsont_bytesrw.decode_string results_jsont s with
  | Ok v -> v
  | Error msg -> failwith msg

let to_string results =
  match Jsont_bytesrw.encode_string results_jsont results with
  | Ok s -> s
  | Error msg -> failwith msg

type search_match = {
  match_start : int;
  match_stop : int;
  match_id : int;
  match_offset : int;
  (* Query-minus-track alignment: constant across frames of a genuine
     match. Frames are only merged when their deltas agree, so a noise
     frame that hits the same track at an unrelated alignment cannot
     extend a match nor skew its pitch estimate. *)
  match_delta : int;
  (* Number of hash votes behind this match, used to weight [match_bin_delta]
     when merging frames: low-vote noise frames should not skew the pitch
     estimate. *)
  match_votes : int;
  match_bin_delta : float;
}

(* Two frames belong to the same match when their alignments agree within
   this many frames (0.3s at the default frame step). *)
let delta_tolerance = 12

let result_of_match ~params ~audio_params
    {
      match_start;
      match_stop;
      match_id;
      match_bin_delta;
      match_offset = _;
      match_delta = _;
      match_votes = _;
    } =
  let t_of_o ofs = float ofs *. audio_params.Audio.frame_step in
  {
    start = t_of_o match_start;
    stop = t_of_o match_stop +. params.frame_length;
    id = string_of_int match_id;
    pitch_semitones =
      match_bin_delta *. 12. /. audio_params.Audio.hashes_bins_per_octave;
  }

type search_entry = Db.match_entry list
type search = Hashes.hash list -> search_entry list

let default_params =
  {
    frame_length = 5.0;
    frame_step = 2.0;
    buffer_size = 10;
    threshold = 7;
    debug = false;
  }

type hash_entry = { pos : int; hash : Hashes.hash; bin : int }

let frames ~params ~audio_params hashes =
  let frame = ref [] in
  let t_of_p pos = float pos *. audio_params.Audio.frame_step in
  let p_of_t t = int_of_float (ceil (t /. audio_params.Audio.frame_step)) in
  let start_pos = ref 0.0 in
  let last_pos = ref 0.0 in
  let size () = !last_pos -. !start_pos in
  let advance () =
    let cut = p_of_t (!start_pos +. params.frame_length) in
    let ret = List.filter (fun { pos; _ } -> pos <= cut) !frame in
    start_pos := !start_pos +. params.frame_step;
    let start = p_of_t !start_pos in
    frame := List.filter (fun { pos; _ } -> start < pos) !frame;
    let min =
      List.fold_left
        (fun min { pos; _ } -> if pos < min then pos else min)
        max_int ret
    in
    let positions = Hashtbl.create (List.length ret) in
    let hashes =
      List.fold_left
        (fun hashes { pos; hash; bin } ->
          Hashtbl.add positions hash { Search_map.rel_pos = pos - min; bin };
          Hashes.HashSet.add hash hashes)
        Hashes.HashSet.empty ret
    in
    Some { Search_map.ofs = min; hashes; positions }
  in
  let rec pull () =
    match hashes () with
    | Some { Hashes.pos; hash; bin } ->
        frame := { pos; hash; bin } :: !frame;
        last_pos := max !last_pos (t_of_p pos);
        if params.frame_length <= size () then advance () else pull ()
    | None -> if !frame <> [] then advance () else None
  in
  pull

let best_match ~debug search_map frame =
  match Search_map.search search_map frame with
  | Some { Search_map.id; delta; count; offset; bin_delta } ->
      if debug then
        Printf.eprintf
          "Best match: id: %d, delta: %d, offset: %d, count: %d, bin delta: %.2f\n\
           %!"
          id delta offset count bin_delta;
      Some
        {
          match_start = frame.Search_map.ofs;
          match_stop = frame.Search_map.ofs;
          match_id = id;
          match_offset = offset;
          match_delta = delta;
          (* [count] is the vote count before the winning entry's last
             increment: the actual number of votes is one more. *)
          match_votes = count + 1;
          match_bin_delta = bin_delta;
        }
  | _ -> None

type buffered_match = {
  mutable count : int;
  mutable mstop : int;
  mutable votes : int;
  mutable bin_sum : float;
}

let same_alignment m m' =
  m.match_id = m'.match_id
  && abs (m.match_delta - m'.match_delta) <= delta_tolerance

let buffered_match ~params content =
  let counts = Hashtbl.create params.buffer_size in
  let keys = ref [] in
  let key_of_match { match_id; match_offset; match_start; _ } =
    (match_id, match_offset, match_start)
  in
  let add_match m =
    keys := m :: !keys;
    Hashtbl.add counts (key_of_match m)
      {
        count = 1;
        mstop = m.match_stop;
        votes = m.match_votes;
        bin_sum = m.match_bin_delta *. float m.match_votes;
      }
  in
  let count m = (Hashtbl.find counts (key_of_match m)).count in
  let merged m =
    let c = Hashtbl.find counts (key_of_match m) in
    {
      m with
      match_stop = c.mstop;
      match_votes = c.votes;
      match_bin_delta = c.bin_sum /. float c.votes;
    }
  in
  let process_match m =
    match List.find_opt (same_alignment m) !keys with
    | Some m' ->
        let c = Hashtbl.find counts (key_of_match m') in
        c.count <- c.count + 1;
        c.mstop <- m.match_stop;
        c.votes <- c.votes + m.match_votes;
        c.bin_sum <- c.bin_sum +. (m.match_bin_delta *. float m.match_votes)
    | None -> add_match m
  in
  for i = 0 to params.buffer_size - 1 do
    match Ringbuffer.get content i with None -> () | Some m -> process_match m
  done;
  List.fold_left
    (fun cur m ->
      match (m, cur) with
      | m, _ when count m < params.threshold -> cur
      | m, Some m' when count m' < count m -> Some (merged m)
      | m, None -> Some (merged m)
      | _ -> cur)
    None !keys

let consolidate matches =
  let h = Hashtbl.create 1024 in
  let process { start; stop; id; pitch_semitones } =
    if Hashtbl.mem h id then begin
      let m = Hashtbl.find h id in
      let m, rem =
        List.partition
          (fun (start', stop', _) ->
            (*            start             stop
             *              |-----------------|
             *   |------------------|
             * start'              stop'
             *
             * But, also:
             *        start    stop
             *          |-------|
             *   |------------------|
             * start'              stop' *)
            (start' <= start && start <= stop')
            || (start <= start' && start' <= stop))
          m
      in
      let start =
        List.fold_left (fun start (start', _, _) -> min start start') start m
      in
      let stop =
        List.fold_left (fun stop (_, stop', _) -> max stop stop') stop m
      in
      (* Merged segments belong to the same alignment: keep the existing
         segment's pitch when there is one. *)
      let pitch_semitones =
        match m with (_, _, pitch) :: _ -> pitch | [] -> pitch_semitones
      in
      Hashtbl.replace h id ([ (start, stop, pitch_semitones) ] @ rem)
    end
    else Hashtbl.add h id [ (start, stop, pitch_semitones) ]
  in
  List.iter process matches;
  let matches =
    Hashtbl.fold
      (fun id positions cur ->
        cur
        @ List.map
            (fun (start, stop, pitch_semitones) ->
              { id; start; stop; pitch_semitones })
            positions)
      h []
  in
  let compare m m' = compare m.start m'.start in
  List.sort compare matches

exception Found of int

let search_hashes ?(params = default_params) ~search ~audio_params hashes =
  let debug = params.debug in
  let is_done = ref false in
  let hashes () =
    match hashes () with
    | None ->
        is_done := true;
        None
    | h -> h
  in
  let search_map = Search_map.init search in
  let frames = frames ~params ~audio_params hashes in
  let matches _ =
    match frames () with
    | Some frame -> best_match ~debug search_map frame
    | None -> None
  in
  let initial_frames = Array.init params.buffer_size matches in
  let buffer = Ringbuffer.init initial_frames in
  let results = ref [] in
  let matches () =
    let ret = buffered_match ~params buffer in
    Ringbuffer.push buffer (matches ());
    ret
  in
  let is_matching = ref false in
  let append_match m =
    match !results with
    | m' :: tail when !is_matching && same_alignment m m' ->
        let votes = m'.match_votes + m.match_votes in
        let bin_delta =
          ((m'.match_bin_delta *. float m'.match_votes)
          +. (m.match_bin_delta *. float m.match_votes))
          /. float votes
        in
        results :=
          {
            m' with
            match_stop = m.match_stop;
            match_votes = votes;
            match_bin_delta = bin_delta;
          }
          :: tail
    | _ -> results := m :: !results
  in
  let rec pull () =
    match matches () with
    | Some m ->
        append_match m;
        is_matching := true;
        pull ()
    | None when not !is_done ->
        is_matching := false;
        pull ()
    | None ->
        List.rev (List.map (result_of_match ~params ~audio_params) !results)
  in
  consolidate (pull ())
