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

(* Params. *)
let quiet = ref false
let input_filename = ref ""

type mode = [ `Store | `Search | `Consolidate ]

let mode : mode ref = ref `Store

let set_mode = function
  | "store" -> mode := `Store
  | "search" -> mode := `Search
  | "consolidate" -> mode := `Consolidate
  | _ -> failwith "Invalid mode!"

let store_id = ref None

type output = [ `Text | `Json ]

let output : output ref = ref `Text

let set_ouptput = function
  | "text" -> output := `Text
  | "json" -> output := `Json
  | _ -> failwith "Invalid output type!"

let debug = ref false

let enable_debug () =
  debug := true;
  quiet := true

let print_profile = ref false

let args =
  List.sort
    (fun (lbl, _, _) (lbl', _, _) -> String.compare lbl lbl')
    ([
       ("-q", Arg.Unit (fun () -> quiet := true), "Quiet mode");
       ("-d", Arg.Unit enable_debug, "Print out debugging information");
       ("-i", Arg.String (fun i -> input_filename := i), "Input file");
       ( "-print-profile",
         Arg.Unit (fun () -> print_profile := true),
         "Output JSON profile" );
       ( "-mode",
         Arg.String set_mode,
         "Mode of operation, one of: \"store\", \"search\" or \"consolidate\". \
          Default: \"store\"" );
       ("-id", Arg.Int (fun i -> store_id := Some i), "Storage ID.");
       ( "-output",
         Arg.String set_ouptput,
         "Output format. One of: \"text\" or \"json\". Default: \"text\"" );
     ]
    @ [
        Args.profile_arg;
        Args.store_arg;
        Args.b1_divisor_arg;
        Args.reassign_arg;
        Args.scheme_arg;
        Args.quads_per_peak_arg;
        Args.max_hash_entries_arg;
        Args.whitening_time_arg;
      ])

let usage = "ithaca <options>"
let search_params () = { (Args.search_params ()) with Search.debug = !debug }

let make_store operations fn =
  let db = Db.make (Args.db_params ()) operations in
  fn db

let time f =
  let start_time = Unix.gettimeofday () in
  let ret = f () in
  let processing_time = Unix.gettimeofday () -. start_time in
  (ret, processing_time)

let print_time t =
  let t = int_of_float t in
  Printf.sprintf "%02d:%02d:%02d" (t / 3600) (t / 60 mod 60) (t mod 60)

let get_hashes ?(probes = false) () =
  let params = Args.audio_params () in
  let hashes =
    Store.hash_file ~probes ~merger:(Args.merger ()) ~params !input_filename
  in
  if !quiet then hashes
  else begin
    let total_hashes = ref 0 in
    let time = ref 0. in
    fun () ->
      match hashes () with
      | Some ({ Hashes.pos; _ } as h) ->
          incr total_hashes;
          time := max !time (float pos *. params.Audio.frame_step);
          Printf.eprintf "\rPosition: %s, generated %d hashes%!"
            (print_time !time) !total_hashes;
          Some h
      | None -> None
  end

let store () =
  let wav = Wav.fopen !input_filename in
  Printf.eprintf "Storing: %s\n%s\n%!"
    (Filename.basename !input_filename)
    (Wav.info wav);
  Wav.close wav;
  let _, processing_time =
    time (fun () ->
        let hashes = get_hashes () in
        let id = match !store_id with Some id -> id | None -> assert false in
        Printf.eprintf "Storing at ID: %i\n%!" id;
        match !output with
        | `Json ->
            make_store Json_store.operations (fun { Db.insert } ->
                insert [ (id, hashes) ])
        | `Text ->
            let db =
              Store.open_db ~profile:(Args.get_profile ())
                ~db_params:(Args.db_params ()) (Args.get_lmdb_path ())
            in
            Store.store db [ (id, hashes) ])
  in
  let ratio = Wav.duration wav /. processing_time in
  if not !quiet then Printf.eprintf "\n";
  Printf.eprintf "Processing time: %s (%01.1fx realtime)\n%!"
    (print_time processing_time)
    ratio

let search_stdout = function
  | [] -> Printf.eprintf "No matches found.. :-(\n%!"
  | l ->
      List.iter
        (fun { Search.start; stop; id; pitch_semitones } ->
          let pitch =
            if abs_float pitch_semitones < 0.05 then ""
            else Printf.sprintf " (pitch: %+.2f semitones)" pitch_semitones
          in
          Printf.eprintf "Found match: %.02f -> %.02f: ID %s%s\n%!" start stop
            id pitch)
        l

let search_csv results =
  Printf.printf "start, stop, ID, pitch_semitones\n";
  List.iter
    (fun { Search.start; stop; id; pitch_semitones } ->
      Printf.printf "%f, %f, %s, %f\n" start stop id pitch_semitones)
    results

let search_json results = print_string (Search.to_string results)

let search () =
  let wav = Wav.fopen !input_filename in
  Printf.eprintf "Search for matches in %s\n%s\n%!"
    (Filename.basename !input_filename)
    (Wav.info wav);
  let duration = Wav.duration wav in
  Wav.close wav;
  let params = search_params () in
  let audio_params = Args.audio_params () in
  let search hashes =
    let get_search { Db.search } = search hashes in
    let operations = Args.lmdb_operations () in
    make_store operations get_search
  in
  let results, processing_time =
    time (fun () ->
        let hashes = get_hashes ~probes:true () in
        Search.search_hashes ~params ~audio_params ~search hashes)
  in
  let results =
    List.map
      (fun ({ Search.stop; _ } as r) -> { r with stop = min duration stop })
      results
  in
  let ratio = Wav.duration wav /. processing_time in
  if not !quiet then Printf.eprintf "\n";
  Printf.eprintf "Processing time: %s (%01.1fx realtime)\n%!"
    (print_time processing_time)
    ratio;
  match !output with
  | `Json -> search_json results
  | `Text -> search_stdout results

let consolidate () =
  let args = Args.anonymous_args () in
  let matches = List.map Search.of_string args in
  let results = Search.consolidate (List.flatten matches) in
  print_string (Search.to_string results)

let check msg fn =
  if fn () then begin
    Printf.eprintf "%s\n" msg;
    Arg.usage args usage;
    exit 1
  end

let check_anon () =
  match Args.anonymous_args () with
  | el :: _ -> check (Printf.sprintf "Invalid argument: %s" el) (fun () -> true)
  | [] -> ()

let check_consolidate () =
  check "Invalid usage: no input given for consolidation!" (fun () ->
      List.length (Args.anonymous_args ()) < 1)

let check_input () =
  check "No inputfile specified!" (fun () -> !input_filename = "")

let check_id () =
  check "No ID specified for storage!" (fun () -> !store_id = None)

let () =
  Printf.eprintf "ithaca -- Audio Fingerprinting in exile\n%!";
  Args.parse ~allow_anon:true ~args usage;
  if !print_profile then begin
    Printf.printf "%s\n" (Args.json_profile ());
    exit 0
  end;
  try
    match !mode with
    | `Consolidate ->
        check_consolidate ();
        consolidate ()
    | `Store ->
        check_anon ();
        check_input ();
        check_id ();
        store ()
    | `Search ->
        check_anon ();
        check_input ();
        search ()
  with e ->
    let bt = Printexc.get_backtrace () in
    Printf.eprintf "\nError while running ithaca:\n%s\n%s\n"
      (Printexc.to_string e) bt;
    exit 1
