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
      if config.jobs > 0 then config.jobs
      else Domain.recommended_domain_count ()
    in
    Printf.eprintf "Indexing %d files (%d jobs)...\n%!" n_files jobs;

    let n_indexed = Atomic.make 0 in
    let n_done = Atomic.make 0 in
    let indexed_mutex = Mutex.create () in
    let indexed_entries = ref [] in
    let prog = Progress.create jobs in

    let items =
      Array.of_list (List.mapi (fun i f -> (i, f, Hashtbl.find id_of f)) files)
    in
    let cursor = Atomic.make 0 in

    let worker domain_idx () =
      let rec loop () =
        if interrupted () then ()
        else begin
          let i = Atomic.fetch_and_add cursor 1 in
          if i >= Array.length items then ()
          else begin
            let _, file, id = items.(i) in
            let dur = Ffmpeg.get_duration file in
            if dur > config.max_duration then
              Progress.update_job prog domain_idx
                (Printf.sprintf "%-40s [skipped: %.0fs]"
                   (Progress.shorten 40 (Filename.basename file))
                   dur)
            else begin
              let on_stage stage =
                Progress.update_job prog domain_idx
                  (Printf.sprintf "%-40s [%s]"
                     (Progress.shorten 40 (Filename.basename file))
                     stage)
              in
              if
                Ithaca_ops.index_file ~ithaca_bin:config.ithaca_bin
                  ~b1_divisor:config.b1_divisor ~reassign:config.reassign
                  ~scheme:config.scheme ~quads_per_peak:config.quads_per_peak
                  ~max_hash_entries:config.max_hash_entries ~on_stage
                  config.db_path file id
              then begin
                Atomic.incr n_indexed;
                Mutex.lock indexed_mutex;
                indexed_entries := (file, id) :: !indexed_entries;
                Mutex.unlock indexed_mutex
              end
            end;
            Atomic.incr n_done;
            Progress.update_header prog
              (Printf.sprintf "[%d/%d] files indexed" (Atomic.get n_done)
                 n_files);
            loop ()
          end
        end
      in
      loop ()
    in
    let workers =
      Array.init (jobs - 1) (fun i -> Domain.spawn (worker (i + 1)))
    in
    worker 0 ();
    Array.iter Domain.join workers;
    Printf.eprintf "\r\027[2K  done: %d/%d files indexed.\n\n%!"
      (Atomic.get n_indexed) n_files;

    let sorted =
      List.sort (fun (a, _) (b, _) -> String.compare a b) !indexed_entries
    in
    Manifest.write config.db_path sorted;
    Printf.printf "Manifest: %s (%d files)\n%!"
      (Manifest.path config.db_path)
      (List.length sorted)
  end
