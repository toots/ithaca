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
  audio_dir : string;
  db_path : string;
  max_duration : float;
  max_files : int; (* 0 = no limit *)
  b1_divisor : int option;
  reassign : bool;
  scheme : string option;
  quads_per_peak : int option;
  max_hash_entries : int option;
  jobs : int; (* 0 = auto *)
}

(* A file hashed by a hashing worker, awaiting storage. Its hashes are spilled
   to [hashes_path] on disk rather than kept in memory, so a backlog does not
   grow the resident set (and swap). *)
type queued = { hashes_path : string; file : string; id : int; dur : float }

let run ?(interrupted = fun () -> false) config =
  let files =
    let all = Ffmpeg.find_audio_files config.audio_dir in
    if config.max_files > 0 then
      let rec take n = function
        | [] -> []
        | _ when n = 0 -> []
        | x :: rest -> x :: take (n - 1) rest
      in
      take config.max_files all
    else all
  in
  let n_files = List.length files in
  Printf.printf "Found %d audio files in %s\n%!" n_files config.audio_dir;
  if n_files = 0 then ()
  else begin
    let id_of = Hashtbl.create n_files in
    List.iteri (fun i f -> Hashtbl.replace id_of f (i + 1)) files;

    let jobs =
      let requested =
        if config.jobs > 0 then config.jobs
        else Domain.recommended_domain_count ()
      in
      max requested 2
    in
    let n_hashers = jobs - 1 in
    Printf.eprintf
      "Indexing %d files (%d hashing workers + 1 store worker)...\n%!" n_files
      n_hashers;

    (* Configure the hashing profile from [config] exactly as the CLI flags do
       (order matters: -scheme sets the quads default max_hash_entries, which an
       explicit -max-hash-entries then overrides). *)
    Option.iter Args.set_b1_divisor config.b1_divisor;
    if config.reassign then Args.set_reassign ();
    Option.iter Args.set_scheme config.scheme;
    Option.iter Args.set_quads_per_peak config.quads_per_peak;
    Option.iter Args.set_max_hash_entries config.max_hash_entries;
    let params = Args.audio_params () in
    let merger = Args.merger () in

    (* One reusable CQT processor per hashing worker, built up front on this
       domain so the non-thread-safe FFTW planning never runs concurrently. *)
    let procs = Array.init n_hashers (fun _ -> Store.make_processor params) in

    let n_indexed = Atomic.make 0 in
    let n_done = Atomic.make 0 in
    let stats_mutex = Mutex.create () in
    let indexed_entries = ref [] in
    let total_audio = ref 0.0 in
    (* seconds of audio actually indexed *)
    let t_start = Unix.gettimeofday () in
    let elapsed () = Unix.gettimeofday () -. t_start in
    let prog = Progress.create jobs in

    let items =
      Array.of_list (List.map (fun f -> (f, Hashtbl.find id_of f)) files)
    in
    let cursor = Atomic.make 0 in

    (* Hashing workers push hashed files here; the store worker drains it.
       The queue is bounded: when hashing outruns storage, producers block on
       [q_space] instead of piling up materialized hashes until we run out of
       memory. Each pending item holds one file's hashes.
       ponytail: fixed [max_pending] ceiling; expose as a config knob if a
       deployment needs a different memory/throughput trade-off. *)
    let max_pending = max 8 (4 * n_hashers) in
    let queue = Queue.create () in
    let q_mutex = Mutex.create () in
    let q_cond = Condition.create () in
    let q_space = Condition.create () in
    let live_hashers = Atomic.make n_hashers in

    let render_header ~done_ ~audio ~itime =
      let db = Stats.disk_bytes config.db_path in
      Printf.sprintf "[%d/%d] indexed · %s · db %s (%s)" done_ n_files
        (Stats.realtime ~audio_s:audio ~wall_s:itime)
        (Stats.human_bytes db)
        (Stats.bytes_per_second db audio)
    in

    (* One of [n_hashers] workers: decode and hash a file in-process (reusing
       its dedicated processor) and enqueue the hashes. *)
    let hasher domain_idx () =
      let fcqt = procs.(domain_idx) in
      let rec loop () =
        if interrupted () then ()
        else begin
          let i = Atomic.fetch_and_add cursor 1 in
          if i >= Array.length items then ()
          else begin
            let file, id = items.(i) in
            let dur = Ffmpeg.get_duration file in
            if dur > config.max_duration then begin
              Progress.update_job prog domain_idx
                (Printf.sprintf "%-40s [skipped: %.0fs]"
                   (Progress.shorten 40 (Filename.basename file))
                   dur);
              Atomic.incr n_done
            end
            else begin
              let on_stage stage =
                Progress.update_job prog domain_idx
                  (Printf.sprintf "%-40s [%s]"
                     (Progress.shorten 40 (Filename.basename file))
                     stage)
              in
              let wav = Filename.temp_file "ithaca_idx" ".wav" in
              let hashes_path =
                Fun.protect
                  ~finally:(fun () -> try Sys.remove wav with _ -> ())
                  (fun () ->
                    on_stage "converting";
                    if not (Ffmpeg.to_wav file wav) then None
                    else begin
                      on_stage "hashing";
                      (* Hash and spill to disk here (this pulls the stream, so
                         all hashing and the WAV read happen now), keeping only
                         the path — not the hashes — in the queue. *)
                      let path = Filename.temp_file "ithaca_hashes" ".json" in
                      Store.write_hashes path
                        (Store.hash_file ~fcqt ~merger ~params wav);
                      Some path
                    end)
              in
              match hashes_path with
              | Some hashes_path ->
                  Mutex.lock q_mutex;
                  (* Back-pressure: wait for the store worker to make room (this
                     also bounds how many spilled files sit on disk). Memory is
                     already released — the hashes are on disk by now. *)
                  while Queue.length queue >= max_pending do
                    Condition.wait q_space q_mutex
                  done;
                  Queue.add { hashes_path; file; id; dur } queue;
                  Condition.signal q_cond;
                  Mutex.unlock q_mutex
              | None -> Atomic.incr n_done
            end;
            loop ()
          end
        end
      in
      loop ();
      (* This producer is done; wake the store worker so it can finish. *)
      ignore (Atomic.fetch_and_add live_hashers (-1));
      Mutex.lock q_mutex;
      Condition.broadcast q_cond;
      Mutex.unlock q_mutex
    in

    (* Sole store worker: drain the queue and insert batches through one open
       database handle (the single LMDB writer). It owns the live stats header —
       it is the one that sees the database grow. *)
    let store_idx = n_hashers in
    let db =
      Store.open_db ~profile:(Args.get_profile ())
        ~db_params:(Args.db_params ()) config.db_path
    in
    let store_worker () =
      let rec loop () =
        Mutex.lock q_mutex;
        while Queue.is_empty queue && Atomic.get live_hashers > 0 do
          Condition.wait q_cond q_mutex
        done;
        let batch = ref [] in
        while not (Queue.is_empty queue) do
          batch := Queue.pop queue :: !batch
        done;
        (* Room freed: wake any hashers blocked on back-pressure. *)
        Condition.broadcast q_space;
        Mutex.unlock q_mutex;
        match List.rev !batch with
        | [] -> () (* queue empty and no live hashers: done *)
        | batch ->
            Progress.update_job prog store_idx
              (Printf.sprintf "storing %d file(s)" (List.length batch));
            (* Read and store one spilled file at a time so the store side never
               holds the whole backlog in memory. *)
            List.iter
              (fun r ->
                Store.store db [ (r.id, Store.read_hashes r.hashes_path) ];
                (try Sys.remove r.hashes_path with _ -> ());
                Atomic.incr n_indexed;
                Atomic.incr n_done;
                Mutex.lock stats_mutex;
                indexed_entries := (r.file, r.id) :: !indexed_entries;
                total_audio := !total_audio +. r.dur;
                Mutex.unlock stats_mutex)
              batch;
            let audio =
              Mutex.lock stats_mutex;
              let a = !total_audio in
              Mutex.unlock stats_mutex;
              a
            in
            Progress.update_header prog
              (render_header ~done_:(Atomic.get n_done) ~audio
                 ~itime:(elapsed ()));
            loop ()
      in
      loop ()
    in

    Progress.update_header prog (render_header ~done_:0 ~audio:0. ~itime:0.);
    let hashers = Array.init n_hashers (fun i -> Domain.spawn (hasher i)) in
    store_worker ();
    Array.iter Domain.join hashers;
    Progress.clear prog;
    Printf.printf "Indexed %d/%d files.\n" (Atomic.get n_indexed) n_files;
    if !total_audio > 0.0 then begin
      let db = Stats.disk_bytes config.db_path in
      Printf.printf "Indexing speed: %s\n"
        (Stats.realtime ~audio_s:!total_audio ~wall_s:(elapsed ()));
      Printf.printf "Database size:  %s (%s)\n" (Stats.human_bytes db)
        (Stats.bytes_per_second db !total_audio)
    end;

    let sorted =
      List.sort (fun (a, _) (b, _) -> String.compare a b) !indexed_entries
    in
    Manifest.write config.db_path sorted;
    Printf.printf "Manifest: %s (%d files)\n%!"
      (Manifest.path config.db_path)
      (List.length sorted)
  end
