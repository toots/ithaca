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

type config = {
  ithaca_bin : string;
  pitch_shift_bin : string;
  db_path : string;
  samples : int;
  clips_per_file : int;
  clip_duration : float;
  pitch_semitones : float array;
  sfx_dir : string;
  sfx_mono : bool;
  sfx_source_lufs : float;
  sfx_mixed_lufs : float;
  samples_dir : string;
  threshold : float;
  jobs : int; (* 0 = auto *)
}

type result = [ `Pass | `Fail | `Skip ]
type pitch_result = { identified : int Atomic.t; tested : int Atomic.t }

let make_pitch_result () =
  { identified = Atomic.make 0; tested = Atomic.make 0 }

let pct n d = if d = 0 then 0.0 else 100.0 *. float n /. float d

let run ?(interrupted = fun () -> false) config entries =
  let n_indexed = List.length entries in
  let id_of = Hashtbl.create n_indexed in
  List.iter (fun (f, id) -> Hashtbl.replace id_of f id) entries;
  let indexed = Array.of_list (List.map fst entries) in

  let sfx_arr = Sample.load_sfx config.sfx_dir in

  let jobs =
    if config.jobs > 0 then config.jobs else Domain.recommended_domain_count ()
  in
  let work_dir =
    Sample.setup_workdir ~prefix:"ithaca_test_" config.samples_dir
  in

  let pitch_results =
    Array.map (fun st -> (st, make_pitch_result ())) config.pitch_semitones
  in

  let basic_identified = Atomic.make 0 in
  let basic_tested = Atomic.make 0 in
  let sfx_identified = Atomic.make 0 in
  let sfx_tested = Atomic.make 0 in
  let has_sfx = Array.length sfx_arr > 0 in
  let n_pitch = Array.length pitch_results in

  (* Summed per-search wall time and the audio it covered, for the live
     "Nx realtime" search-speed figure. Each search runs on a clip of
     [clip_duration] seconds. *)
  let stats_mutex = Mutex.create () in
  let total_search_time = ref 0.0 in
  let n_searches () =
    Atomic.get basic_tested + Atomic.get sfx_tested
    + Array.fold_left
        (fun acc (_, pr) -> acc + Atomic.get pr.tested)
        0 pitch_results
  in
  let search_realtime () =
    let audio_s = float (n_searches ()) *. config.clip_duration in
    Mutex.lock stats_mutex;
    let wall_s = !total_search_time in
    Mutex.unlock stats_mutex;
    Stats.realtime ~audio_s ~wall_s
  in
  let search clip =
    let t0 = Unix.gettimeofday () in
    let r =
      Ithaca_ops.search_wav ~ithaca_bin:config.ithaca_bin config.db_path clip
    in
    let dt = Unix.gettimeofday () -. t0 in
    Mutex.lock stats_mutex;
    total_search_time := !total_search_time +. dt;
    Mutex.unlock stats_mutex;
    r
  in
  let margin = 5.0 in
  let min_usable_dur = config.clip_duration +. (2.0 *. margin) in

  let rng_main = Random.State.make_self_init () in
  Sample.shuffle rng_main indexed;
  let test_files =
    Array.sub indexed 0 (min config.samples (Array.length indexed))
  in
  let n_test = Array.length test_files in

  let sfx_desc =
    if Array.length sfx_arr > 0 then
      Printf.sprintf " + sfx mixing (%d files)" (Array.length sfx_arr)
    else ""
  in
  Printf.eprintf "Testing: %d clips × %g s each, %d/%d files%s (%d jobs)...\n%!"
    config.clips_per_file config.clip_duration n_test n_indexed sfx_desc jobs;

  let test_n_done = Atomic.make 0 in
  let clip_counter = Atomic.make 0 in
  let header_lines =
    1 (* files tested *) + 1 (* basic *)
    + (if has_sfx then 1 else 0)
    + (if n_pitch > 0 then 1 + n_pitch else 0)
    + 1 (* search speed *)
  in
  let prog = Progress.create ~header_lines jobs in
  let test_cursor = Atomic.make 0 in

  let render_stats () =
    let b = Buffer.create 256 in
    Buffer.add_string b
      (Printf.sprintf "[%d/%d] files tested" (Atomic.get test_n_done) n_test);
    let bi = Atomic.get basic_identified and bt = Atomic.get basic_tested in
    Buffer.add_string b
      (Printf.sprintf "\nBasic: %d / %d (%.1f%%)" bi bt (pct bi bt));
    if has_sfx then begin
      let si = Atomic.get sfx_identified and st = Atomic.get sfx_tested in
      Buffer.add_string b
        (Printf.sprintf "\nSFX:   %d / %d (%.1f%%)" si st (pct si st))
    end;
    if n_pitch > 0 then begin
      Buffer.add_string b "\nPitch-shifted:";
      Array.iter
        (fun (st, pr) ->
          let ti = Atomic.get pr.identified and tt = Atomic.get pr.tested in
          Buffer.add_string b
            (Printf.sprintf "\n  %+.2f st: %d / %d (%.1f%%)" st ti tt
               (pct ti tt)))
        pitch_results
    end;
    Buffer.add_string b (Printf.sprintf "\nSearch: %s" (search_realtime ()));
    Buffer.contents b
  in

  let worker domain_idx () =
    let rng = Random.State.make_self_init () in
    let rec loop () =
      if interrupted () then ()
      else begin
        let i = Atomic.fetch_and_add test_cursor 1 in
        if i >= n_test then ()
        else begin
          let file = test_files.(i) in
          let id = Hashtbl.find id_of file in
          let dur = Ffmpeg.get_duration file in
          if dur >= min_usable_dur then begin
            for clip_n = 0 to config.clips_per_file - 1 do
              let start =
                margin +. Random.State.float rng (dur -. min_usable_dur)
              in
              let clip_id = Atomic.fetch_and_add clip_counter 1 in
              let clip =
                Filename.concat work_dir (Printf.sprintf "clip_%d.wav" clip_id)
              in
              let show stage =
                Progress.update_job prog domain_idx
                  (Printf.sprintf "%-32s  clip %d  [%s]"
                     (Progress.shorten 32 (Filename.basename file))
                     (clip_n + 1) stage)
              in
              show "extracting";
              if Ffmpeg.extract_clip file clip start config.clip_duration then begin
                show "searching";
                let results = search clip in
                Atomic.incr basic_tested;
                (match results with
                | { Search.id = s; _ } :: _ when int_of_string_opt s = Some id
                  ->
                    Atomic.incr basic_identified
                | _ -> ());
                Progress.update_header prog (render_stats ());
                if Array.length sfx_arr > 0 then begin
                  let sfx =
                    sfx_arr.(Random.State.int rng (Array.length sfx_arr))
                  in
                  let sfx_dur = Ffmpeg.get_duration sfx in
                  let sfx_offset =
                    let max_off =
                      Float.max 0. (sfx_dur -. config.clip_duration)
                    in
                    if max_off > 0. then Random.State.float rng max_off else 0.
                  in
                  let mixed =
                    Filename.concat work_dir
                      (Printf.sprintf "clip_%d_sfx.wav" clip_id)
                  in
                  show "mixing sfx";
                  if
                    Ffmpeg.mix_sfx ~offset:sfx_offset ~mono:config.sfx_mono
                      ~source_lufs:config.sfx_source_lufs
                      ~sfx_lufs:config.sfx_mixed_lufs clip sfx mixed
                  then begin
                    show "searching sfx";
                    let results = search mixed in
                    Atomic.incr sfx_tested;
                    (match results with
                    | { Search.id = s; _ } :: _
                      when int_of_string_opt s = Some id ->
                        Atomic.incr sfx_identified
                    | _ -> ());
                    Progress.update_header prog (render_stats ());
                    if config.samples_dir = "" then
                      try Sys.remove mixed with _ -> ()
                  end
                end;
                Array.iteri
                  (fun pitch_idx (semitones, pr) ->
                    let shifted =
                      Filename.concat work_dir
                        (Printf.sprintf "clip_%d_p%d.wav" clip_id pitch_idx)
                    in
                    show (Printf.sprintf "pitch %+.2f" semitones);
                    if
                      Shell.run_cmd "%s %s %s %g"
                        (Filename.quote config.pitch_shift_bin)
                        (Filename.quote clip) (Filename.quote shifted) semitones
                    then begin
                      show (Printf.sprintf "searching pitch %+.2f" semitones);
                      let results = search shifted in
                      Atomic.incr pr.tested;
                      (match results with
                      | { Search.id = s; _ } :: _
                        when int_of_string_opt s = Some id ->
                          Atomic.incr pr.identified
                      | _ -> ());
                      Progress.update_header prog (render_stats ());
                      if config.samples_dir = "" then
                        try Sys.remove shifted with _ -> ()
                    end)
                  pitch_results;
                if config.samples_dir = "" then
                  try Sys.remove clip with _ -> ()
              end
            done
          end;
          Atomic.incr test_n_done;
          Progress.update_header prog (render_stats ());
          loop ()
        end
      end
    in
    loop ()
  in
  Progress.update_header prog (render_stats ());
  let workers =
    Array.init (jobs - 1) (fun i -> Domain.spawn (worker (i + 1)))
  in
  worker 0 ();
  Array.iter Domain.join workers;
  Progress.clear prog;

  Printf.printf "=== Integration test results ===\n\n";
  Printf.printf "Database:       %s\n" config.db_path;
  Printf.printf "Files indexed:  %d\n" n_indexed;
  Printf.printf "Files sampled:  %d\n" n_test;
  Printf.printf "Clips tested:   %d\n\n" (Atomic.get basic_tested);
  Printf.printf "Basic identification: %d / %d  (%.1f%%)\n"
    (Atomic.get basic_identified)
    (Atomic.get basic_tested)
    (pct (Atomic.get basic_identified) (Atomic.get basic_tested));

  if Array.length sfx_arr > 0 then
    Printf.printf "SFX identification:   %d / %d  (%.1f%%)\n"
      (Atomic.get sfx_identified)
      (Atomic.get sfx_tested)
      (pct (Atomic.get sfx_identified) (Atomic.get sfx_tested));

  if Array.length pitch_results > 0 then begin
    Printf.printf "\nPitch-shifted identification:\n";
    Array.iter
      (fun (st, pr) ->
        let identified = Atomic.get pr.identified in
        let tested = Atomic.get pr.tested in
        Printf.printf "  %+.2f semitones: %d / %d identified (%.1f%%)\n" st
          identified tested (pct identified tested))
      pitch_results
  end;
  Printf.printf "\nSearch speed: %s\n%!" (search_realtime ());

  let t = Atomic.get basic_tested in
  if t = 0 then `Skip
  else
    let rate = float (Atomic.get basic_identified) /. float t in
    if rate < config.threshold then (
      Printf.printf "FAIL: %.1f%% < required %.0f%%\n%!" (rate *. 100.0)
        (config.threshold *. 100.0);
      `Fail)
    else (
      Printf.printf "PASS: %.1f%% >= %.0f%%\n%!" (rate *. 100.0)
        (config.threshold *. 100.0);
      `Pass)
