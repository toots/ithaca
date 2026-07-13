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

type slice = Distributed_t.slice = { offset : float; start : int; length : int }

type slices = Distributed_t.slices = {
  header : Wav.header;
  slices : slice list;
}

let usage = "ithaca_distributed <args>"

type mode = [ `Enqueue | `Hash | `Search | `Finalize ]

let mode : mode ref = ref `Enqueue

let set_mode = function
  | "enqueue" -> mode := `Enqueue
  | "hash" -> mode := `Hash
  | "search" -> mode := `Search
  | "finalize" -> mode := `Finalize
  | _ -> failwith "Invalid mode!"

let header = ref ""
let hashes = ref ""
let debug = ref false
let enable_debug () = debug := true
let filename = ref ""
let matches = ref ""
let batch_size = ref 60.
let batch_step = ref 50.
let offset = ref 0.

let args =
  [
    ("-i", Arg.String (fun i -> filename := i), "Input file");
    ("-d", Arg.Unit enable_debug, "Print out debugging information");
    ("-header", Arg.String (fun h -> header := h), "WAV header");
    ( "-hashes",
      Arg.String (fun h -> hashes := h),
      "Hashes file containing JSON array" );
    ( "-batch_size",
      Arg.Float (fun s -> batch_size := s),
      Printf.sprintf "Slices batch size. (Default: %.02f)" !batch_size );
    ( "-batch_step",
      Arg.Float (fun s -> batch_step := s),
      Printf.sprintf "Slices batch step. (Default: %.02f)" !batch_step );
    ("-offset", Arg.Float (fun o -> offset := o), "Offset for partial match.");
    ("-matches", Arg.String (fun r -> matches := r), "Raw match data.");
    ( "-mode",
      Arg.String set_mode,
      "Mode of operation, one of: \"enqueue\", \"search\", \"hash\" or \
       \"finalize\". Default: \"enqueue\"" );
  ]
  @ [ Args.profile_arg ] @ [ Args.store_arg ]

let search_params () = { (Args.search_params ()) with Search.debug = !debug }

let enqueue () =
  if !filename = "" then begin
    Printf.eprintf "No filename specified!\n";
    Arg.usage args usage;
    exit 1
  end;
  let wav = Wav.fopen !filename in
  let ({ Wav.channels; sample_rate; bits_per_sample } as header) =
    Wav.header wav
  in
  let data_offset = Wav.data_offset wav in
  Printf.eprintf
    "Input detected: PCM WAVE %d channels, %d Hz, %d bits, data offset: %d\n%!"
    channels sample_rate bits_per_sample data_offset;
  let data_length = Wav.data_length wav in
  let batch_length = Wav.duration_length wav !batch_size in
  let step_length = Wav.duration_length wav !batch_step in
  (* Let's not decode slices of less than this. *)
  let min_length = Wav.duration_length wav 12. in
  let rec f slices start =
    let length =
      if data_length <= start + batch_length then data_length - start
      else batch_length
    in
    if length < min_length then slices
    else
      let offset = Wav.length_duration wav (start - data_offset) in
      let slices = { offset; start; length } :: slices in
      if data_length <= start + step_length then slices
      else f slices (start + step_length)
  in
  let slices =
    List.sort (fun s s' -> compare s.start s'.start) (f [] data_offset)
  in
  let buf = Buffer.create 4096 in
  Distributed_j.write_slices buf { header; slices };
  print_string (Buffer.contents buf)

let make_storage fn =
  let operations = Args.lmdb_operations () in
  let db = Db.make (Args.db_params ()) operations in
  fn db

let hashes_of_wav ~audio_params filename header =
  let header = Wav_j.header_of_string header in
  let length = (Unix.stat filename).Unix.st_size in
  let open_wav merger =
    let ic = open_in_bin filename in
    Audio.hash_wav ~merger ~params:audio_params
      (Wav.from_raw ~header ~length ic)
  in
  match Args.merger () with
  | Audio.Single merger -> open_wav merger
  | Audio.Both ->
      if header.Wav_t.channels = 1 then open_wav Audio.mono_merger
      else
        Hashes.merge_parallel
          (open_wav Audio.mono_merger)
          (open_wav Audio.center_merger)

let hash () =
  if !header = "" || !filename = "" then begin
    Printf.eprintf "Invalid usage!\n";
    Arg.usage args usage;
    exit 1
  end;
  let audio_params = Args.audio_params () in
  let hashes = hashes_of_wav ~audio_params !filename !header in
  let buf = Buffer.create 4096 in
  let entries =
    List.map
      (fun { Hashes.pos; hash; bin } -> { Distributed_t.pos; hash; bin })
      (IStream.pull hashes)
  in
  Distributed_j.write_hashes buf entries;
  print_string (Buffer.contents buf)

let hashes_of_hashes filename =
  let ch = open_in filename in
  let buf = Buffer.create 1024 in
  let rec f () =
    try
      Buffer.add_channel buf ch 1024;
      f ()
    with End_of_file -> ()
  in
  f ();
  close_in ch;
  let hashes = Buffer.contents buf in
  IStream.make
    (List.map
       (fun { Distributed_t.pos; hash; bin } -> { Hashes.pos; hash; bin })
       (Distributed_j.hashes_of_string hashes))

let search () =
  let audio_params = Args.audio_params () in
  let hashes =
    match (!header, !filename, !hashes) with
    | header, filename, _ when header <> "" && filename <> "" ->
        hashes_of_wav ~audio_params filename header
    | _, _, hashes when hashes <> "" -> hashes_of_hashes hashes
    | _ ->
        Printf.eprintf "Invalid usage!\n";
        Arg.usage args usage;
        exit 1
  in
  let search hashes =
    let get_search { Db.search } = search hashes in
    make_storage get_search
  in
  let params = search_params () in
  let matches = Search.search_hashes ~params ~audio_params ~search hashes in
  let matches =
    List.map
      (fun ({ Search.start; stop } as m) ->
        { m with Search.start = start +. !offset; stop = stop +. !offset })
      matches
  in
  let buf = Buffer.create 4096 in
  Search_j.write_results buf matches;
  print_string (Buffer.contents buf)

let finalize () =
  if !matches = "" then begin
    Printf.eprintf "Missing raw match data!\n";
    Arg.usage args usage;
    exit 1
  end;
  let matches = Search_j.results_of_string !matches in
  let results = Search.consolidate matches in
  let buf = Buffer.create 4096 in
  Distributed_j.write_results buf results;
  print_string (Buffer.contents buf)

let () =
  Printf.eprintf "ithaca_distributed -- Audio Fingerprinting in exile\n%!";
  Printexc.record_backtrace true;
  Args.parse ~args usage;
  match !mode with
  | `Enqueue -> enqueue ()
  | `Search -> search ()
  | `Hash -> hash ()
  | `Finalize -> finalize ()
