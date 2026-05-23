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

(* Generate data for gnuplot. *)

let open_tmp_chan () = Filename.open_temp_file "ithaca-plot" "dat"
let fcqd_fname = ref None
let pairs_fname = ref None
let peaks_fname = ref None
let labels_fname = ref None
let get_some x = match x with Some x -> x | None -> assert false
let unlink fname = try Unix.unlink fname with _ -> ()

let cleanup () =
  let unlink_some x = match x with Some x -> unlink x | None -> () in
  unlink_some !fcqd_fname;
  unlink_some !pairs_fname;
  unlink_some !peaks_fname;
  unlink_some !labels_fname

let ensure f g =
  try
    let ret = g () in
    f ();
    ret
  with e ->
    f ();
    raise e

let generate_data wav =
  let params = Args.audio_params () in
  let fname, fcqt_chan = open_tmp_chan () in
  fcqd_fname := Some fname;
  let pos = ref 0 in
  let cqt_size = ref 0 in
  let t_of_f n = float n *. params.Audio.frame_step in
  let cqt row =
    cqt_size := Array.length row;
    Array.iteri (Printf.fprintf fcqt_chan "%f\t%i\t%f\n" (t_of_f !pos)) row;
    incr pos
  in
  let fname, peaks_chan = open_tmp_chan () in
  peaks_fname := Some fname;
  let fname, labels_chan = open_tmp_chan () in
  labels_fname := Some fname;
  let peaks l =
    List.iter
      (fun (x, y) ->
        Printf.fprintf peaks_chan "%f\t%i\n" (t_of_f x) y;
        Printf.fprintf labels_chan "%f\t%i\t(%i,%i)\n" (t_of_f x) (y + 4) x y)
      l
  in
  let fname, pairs_chan = open_tmp_chan () in
  pairs_fname := Some fname;
  let pairs l =
    List.iter
      (fun ((x1, y1), (x2, y2)) ->
        Printf.fprintf pairs_chan "%f\t%i\t%f\t%i\n" (t_of_f x1) y1 (t_of_f x2)
          y2)
      l
  in
  let instruments =
    { Audio.cqt = Some cqt; peaks = Some peaks; pairs = Some pairs }
  in
  let hashes =
    Audio.hash_wav ~merger:(Args.merger ()) ~instruments ~params wav
  in
  ignore (IStream.pull hashes);
  close_out fcqt_chan;
  close_out peaks_chan;
  close_out pairs_chan;
  close_out labels_chan;
  !cqt_size

let input_filename = ref ""
let output_filename = ref ""
let plot_labels = ref false

let args =
  [
    ("-labels", Arg.Unit (fun () -> plot_labels := true), "Plot peak labels");
    ("-i", Arg.String (fun i -> input_filename := i), "Input file");
    ("-o", Arg.String (fun o -> output_filename := o), "Output file");
  ]
  @ [ Args.profile_arg ]

let usage = "plot -i <input.wav> -o <output.png>"

let () =
  Args.parse args usage;
  if !input_filename = "" || !output_filename = "" then begin
    Printf.eprintf "%s\n%!" usage;
    exit 1
  end;
  Printf.printf "Generating fingerpring data for %s..\n%!" !input_filename;
  let wav = Wav.fopen !input_filename in
  let len = Wav.duration wav in
  let code =
    ensure
      (fun () ->
        Wav.close wav;
        cleanup ())
      (fun () ->
        let y_len = generate_data wav in
        Printf.printf "Generating %s..\n%!" !output_filename;
        begin try Unix.unlink !output_filename with _ -> ()
        end;
        let gnuplot_fname, chan = open_tmp_chan () in
        let labels_plot =
          if !plot_labels then
            Printf.sprintf
              ", '%s' with labels textcolor \"red\" font \"Symbol,8\" notitle"
              (get_some !labels_fname)
          else ""
        in
        ensure
          (fun () -> unlink gnuplot_fname)
          (fun () ->
            Printf.fprintf chan "set terminal png font arial 14 size 800,600\n";
            Printf.fprintf chan "set output '%s'\n" !output_filename;
            Printf.fprintf chan "set xlabel \"Time (sec)\"\n";
            Printf.fprintf chan "set ylabel \"Frequency Bin\"\n";
            Printf.fprintf chan "set title \"Fingerprint data for %s\"\n"
              (Filename.basename !input_filename);
            Printf.fprintf chan "set xrange [0:%f]\n" len;
            Printf.fprintf chan "set yrange [0:%i]\n" y_len;
            Printf.fprintf chan "unset colorbox\n";
            Printf.fprintf chan "set tics out\n";
            Printf.fprintf chan
              "plot '%s' with image notitle, '%s' using 1:2:($3-$1):($4-$2) \
               with vectors notitle nohead lw 0.5 lc \"grey\"%s, '%s' w p ls 3 \
               notitle"
              (get_some !fcqd_fname) (get_some !pairs_fname) labels_plot
              (get_some !peaks_fname);
            close_out chan;
            Sys.command (Printf.sprintf "gnuplot %s" gnuplot_fname)))
  in
  exit code
