let interrupted = Atomic.make false
let is_interrupted () = Atomic.get interrupted
let default_pitch_semitones = [| 0.5; -0.5; 1.0; -1.0; 1.5; -1.5; 2.0; -2.0 |]
let exit_result = function `Pass | `Skip -> exit 0 | `Fail -> exit 1

(* ── Index command ────────────────────────────────────────────────────────── *)

let cmd_index argv =
  let c =
    Index.
      {
        ithaca_bin = Paths.ithaca;
        audio_dir = "";
        db_path = "";
        max_duration = 1200.0;
        max_files = 0;
        b1_divisor = None;
        reassign = false;
        scheme = None;
        quads_per_peak = None;
        max_hash_entries = None;
        jobs = 0;
      }
  in
  let c = ref c in
  let args =
    [
      ( "--ithaca",
        Arg.String (fun s -> c := { !c with ithaca_bin = s }),
        "PATH  ithaca binary" );
      ( "--audio-dir",
        Arg.String (fun s -> c := { !c with audio_dir = s }),
        "DIR   Audio file directory" );
      ( "--db",
        Arg.String (fun s -> c := { !c with db_path = s }),
        "PATH  Output database path" );
      ( "--max-duration",
        Arg.Float (fun f -> c := { !c with max_duration = f }),
        "SECS  Skip files longer than this (default 1200)" );
      ( "--max-files",
        Arg.Int (fun n -> c := { !c with max_files = n }),
        "N  Limit number of files to index (default: no limit)" );
      ( "--b1-divisor",
        Arg.Int (fun n -> c := { !c with b1_divisor = Some n }),
        "N  Divisor for b̂₁ in the hash (default: ithaca's default)" );
      ( "--reassign",
        Arg.Unit (fun () -> c := { !c with reassign = true }),
        "  Enable frequency reassignment (sharper peaks, ~6x slower)" );
      ( "--scheme",
        Arg.String (fun s -> c := { !c with scheme = Some s }),
        "NAME  Hashing scheme: pairs (default) or quads" );
      ( "--quads-per-peak",
        Arg.Int (fun n -> c := { !c with quads_per_peak = Some n }),
        "N  Quads scheme: max quads per peak (lower = smaller/faster DB)" );
      ( "--max-hash-entries",
        Arg.Int (fun n -> c := { !c with max_hash_entries = Some n }),
        "N  Drop hashes exceeding this many entries (0 = no limit)" );
      ( "--jobs",
        Arg.Int (fun n -> c := { !c with jobs = n }),
        "N  Parallel jobs (default: Domain.recommended_domain_count ())" );
    ]
  in
  Arg.parse_argv argv args (fun _ -> ()) "integration_test index [options]";
  if !c.audio_dir = "" then (
    Printf.printf "SKIP: --audio-dir not provided\n%!";
    exit 0);
  if !c.db_path = "" then (
    Printf.eprintf "Error: --db required\n%!";
    exit 1);
  if not (Sys.file_exists !c.audio_dir && Sys.is_directory !c.audio_dir) then (
    Printf.printf "SKIP: audio dir '%s' not found\n%!" !c.audio_dir;
    exit 0);
  let scheme = Option.value ~default:"pairs" !c.scheme in
  let opt name = Option.map (Printf.sprintf "%s=%d" name) in
  let params =
    List.filter_map Fun.id
      [
        opt "b1-divisor" !c.b1_divisor;
        (if scheme = "quads" then opt "quads-per-peak" !c.quads_per_peak
         else None);
        (if scheme = "quads" then opt "max-hash-entries" !c.max_hash_entries
         else None);
        (if !c.reassign then Some "reassign=true" else None);
      ]
  in
  Printf.printf "Hashing scheme: %s%s\n%!" scheme
    (if params = [] then ""
     else Printf.sprintf " (%s)" (String.concat ", " params));
  Index.run ~interrupted:is_interrupted !c

(* ── Test command ─────────────────────────────────────────────────────────── *)

let cmd_test argv =
  let c =
    ref
      Fingerprint_test.
        {
          ithaca_bin = Paths.ithaca;
          pitch_shift_bin = Paths.pitch_shift;
          db_path = "";
          samples = 50;
          clips_per_file = 3;
          clip_duration = 20.0;
          pitch_semitones = default_pitch_semitones;
          sfx_dir = "";
          sfx_mono = false;
          sfx_source_lufs = -14.0;
          sfx_mixed_lufs = -10.0;
          samples_dir = "";
          threshold = 0.80;
          jobs = 0;
        }
  in
  let audio_dir = ref "" in
  let no_pitch = ref false in
  let args =
    [
      ( "--ithaca",
        Arg.String (fun s -> c := { !c with ithaca_bin = s }),
        "PATH  ithaca binary" );
      ("--audio-dir", Arg.Set_string audio_dir, "DIR   Audio file directory");
      ( "--db",
        Arg.String (fun s -> c := { !c with db_path = s }),
        "PATH  Database path" );
      ( "--sfx-dir",
        Arg.String (fun s -> c := { !c with sfx_dir = s }),
        "DIR   Sound effects directory for mixing" );
      ( "--sfx-mono",
        Arg.Bool (fun b -> c := { !c with sfx_mono = b }),
        "BOOL  Convert SFX to mono before mixing (default: false)" );
      ( "--sfx-source-lufs",
        Arg.Float (fun f -> c := { !c with sfx_source_lufs = f }),
        "LUFS  Target loudness for the source audio (default: -14)" );
      ( "--sfx-mixed-lufs",
        Arg.Float (fun f -> c := { !c with sfx_mixed_lufs = f }),
        "LUFS  Target loudness for the SFX (default: -10)" );
      ( "--samples-dir",
        Arg.String (fun s -> c := { !c with samples_dir = s }),
        "DIR   Save prepared samples here for manual inspection (default: \
         temporary)" );
      ( "--jobs",
        Arg.Int (fun n -> c := { !c with jobs = n }),
        "N  Parallel jobs (default: Domain.recommended_domain_count ())" );
      ( "--samples",
        Arg.Int (fun n -> c := { !c with samples = n }),
        "N  Files to sample (default 50)" );
      ( "--clips",
        Arg.Int (fun n -> c := { !c with clips_per_file = n }),
        "N  Clips per file (default 3)" );
      ( "--clip-duration",
        Arg.Float (fun f -> c := { !c with clip_duration = f }),
        "SECS  Clip duration (default 20)" );
      ( "--no-pitch",
        Arg.Set no_pitch,
        "  Skip pitch-shift tests (sub-semitone by default)" );
      ( "--threshold",
        Arg.Float (fun f -> c := { !c with threshold = f }),
        "RATE  Required pass rate 0.0-1.0 (default 0.80)" );
    ]
  in
  Arg.parse_argv argv args (fun _ -> ()) "integration_test test [options]";
  if !c.db_path = "" then (
    Printf.eprintf "Error: --db required\n%!";
    exit 1);
  if !audio_dir = "" then (
    Printf.printf "SKIP: --audio-dir not provided\n%!";
    exit 0);
  let mpath = Manifest.path !c.db_path in
  if not (Sys.file_exists mpath) then (
    Printf.eprintf "Error: manifest %s not found — run 'index' first\n%!" mpath;
    exit 1);
  (try
     let p = Lmdb_store.get_profile !c.db_path in
     let params =
       if p.Profile_t.scheme = "quads" then
         Printf.sprintf "quads-per-peak=%d, max-hash-entries=%d, reassign=%b"
           p.Profile_t.quads_per_peak p.Profile_t.max_hash_entries
           p.Profile_t.reassign
       else Printf.sprintf "reassign=%b" p.Profile_t.reassign
     in
     Printf.printf "Hashing scheme: %s (%s)\n%!" p.Profile_t.scheme params
   with _ -> ());
  if !no_pitch then c := { !c with pitch_semitones = [||] };
  let entries = Manifest.read !c.db_path in
  Printf.printf "Loaded manifest: %d indexed files\n%!" (List.length entries);
  exit_result (Fingerprint_test.run ~interrupted:is_interrupted !c entries)

(* ── Dispatch ─────────────────────────────────────────────────────────────── *)

let () =
  Sys.set_signal Sys.sigint
    (Sys.Signal_handle
       (fun _ ->
         Atomic.set interrupted true;
         Printf.eprintf "\nInterrupted.\n%!";
         exit 1));
  let subcmd = if Array.length Sys.argv > 1 then Sys.argv.(1) else "" in
  let rest =
    Array.append
      [| Sys.argv.(0) |]
      (Array.sub Sys.argv 2 (max 0 (Array.length Sys.argv - 2)))
  in
  try
    match subcmd with
    | "index" -> cmd_index rest
    | "test" -> cmd_test rest
    | _ ->
        Printf.eprintf
          "Usage:\n\
          \  integration_test index     --ithaca PATH --audio-dir DIR --db \
           PATH [options]\n\
          \  integration_test test      --audio-dir DIR --db PATH [options]\n";
        exit 1
  with
  | Arg.Help msg ->
      print_string msg;
      exit 0
  | Arg.Bad msg ->
      Printf.eprintf "%s" msg;
      exit 1
