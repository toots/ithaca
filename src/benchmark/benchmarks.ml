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

let time f =
  let t = Sys.time () in
  f ();
  Printf.printf "Excution time: %.02f sec\n%!" (Sys.time () -. t)

let benchmark_fcqt ~reassign () =
  let params =
    {
      Fcqt.min_freq = 640.0;
      max_freq = 1700.0;
      bins_per_octave = 24.0;
      samplerate = 11025.0;
      step = 0.025;
      reassign;
    }
  in
  let pi = 4.0 *. atan 1.0 in
  let sin n =
    (* D5 *)
    sin (2.0 *. pi *. float n *. 587.33 /. 11025.0)
  in
  let repeat = 10000 in
  let fcqt = Fcqt.init params in
  let data = Array.init (Fcqt.sample_size fcqt) sin in
  Printf.printf
    "--> Benchmarking %i FCQT transforms of %i samples with reassign: %b.\n%!"
    repeat (Array.length data) reassign;
  time (fun () ->
      for n = 1 to repeat do
        begin try ignore (Fcqt.execute fcqt data)
        with Fcqt.Need_more_data -> ()
        end
      done)

let benchmark_hash () =
  let repeat = 1000000 in
  Printf.printf "--> Benchmarking %i hashes.\n%!" repeat;
  time (fun () ->
      for n = 1 to repeat do
        ignore (Hashes.hash (2, 3) (4, 5))
      done)

let hashes = ref []

let benchmark_hashing () =
  let wav =
    Wav.fopen
      (Filename.concat (Filename.dirname Sys.executable_name) "orig.wav")
  in
  let hasher = ref None in
  let rec exec () =
    match !hasher with
    | Some f -> begin
        match f () with
        | Some hash ->
            hashes := hash :: !hashes;
            exec ()
        | None -> ()
      end
    | None -> assert false
  in
  Printf.printf "--> Benchmarking hashing of %.02fs of audio.\n%!"
    (Wav.duration wav);
  Printf.printf "~~~> Initialization\n%!";
  time (fun () -> hasher := Some (Audio.hash_wav wav));
  Printf.printf "~~~> Execution\n%!";
  time exec;
  Printf.printf "Generated a total of %i hashes.\n%!" (List.length !hashes);
  Wav.close wav

let benchmark_insert db =
  let size = 10 in
  Printf.printf "--> Benchmarking insertion of %i hashes\n%!"
    (size * List.length !hashes);
  time (fun () ->
      for i = 0 to size - 1 do
        let hashes_stream = IStream.make !hashes in
        db.Db.insert [ (1234, hashes_stream) ]
      done)

let benchmark_search db =
  Printf.printf "--> Benchmarking search of %i hashes\n%!" (List.length !hashes);
  let hashes = List.map (fun { Hashes.hash; _ } -> hash) !hashes in
  time (fun () -> ignore (db.Db.search hashes))

let wrap f =
  let tmpfile = Filename.temp_file "ithaca-test" ".db" in
  let rm () = try Sys.remove tmpfile with _ -> () in
  try
    let ret = f tmpfile in
    rm ();
    ret
  with e ->
    rm ();
    raise e

let () =
  Printf.printf "\n\nRunning benchmark suite..\n%!";
  benchmark_fcqt ~reassign:false ();
  Printf.printf "\n%!";
  benchmark_fcqt ~reassign:true ();
  Printf.printf "\n%!";
  benchmark_hash ();
  Printf.printf "\n%!";
  benchmark_hashing ();
  Printf.printf "\n%!";
  wrap (fun tmpfile ->
      let params = { Db.max_id_per_hash = 1024; max_pos_per_hash = 50 } in
      let db = Db.make params (Lmdb_store.operations tmpfile) in
      benchmark_insert db;
      Printf.printf "\n%!";
      benchmark_search db;
      Printf.printf "\n%!");
  Printf.printf "Done!\n\n%!"
