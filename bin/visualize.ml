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

let colormap v =
  let v = Float.max 0. (Float.min 1. v) in
  if v < 0.333 then
    let t = v /. 0.333 in
    (int_of_float (t *. 255.), 0, 0)
  else if v < 0.667 then
    let t = (v -. 0.333) /. 0.334 in
    (255, int_of_float (t *. 255.), 0)
  else
    let t = (v -. 0.667) /. 0.333 in
    (255, 255, int_of_float (t *. 255.))

let draw_line img ~width ~height x0 y0 x1 y1 r g b =
  let dx = abs (x1 - x0) and dy = abs (y1 - y0) in
  let sx = if x0 < x1 then 1 else -1 in
  let sy = if y0 < y1 then 1 else -1 in
  let err = ref (dx - dy) in
  let x = ref x0 and y = ref y0 in
  let running = ref true in
  while !running do
    if !x >= 0 && !x < width && !y >= 0 && !y < height then
      Image.write_rgb img !x !y r g b;
    if !x = x1 && !y = y1 then running := false
    else begin
      let e2 = 2 * !err in
      if e2 > -dy then (
        err := !err - dy;
        x := !x + sx);
      if e2 < dx then (
        err := !err + dx;
        y := !y + sy)
    end
  done

let () =
  let dump_peaks = ref false in
  let args =
    [
      ( "--dump-peaks",
        Arg.Set dump_peaks,
        "  Write peak coordinates to a text file" );
    ]
    @ [
        Args.profile_arg;
        Args.store_arg;
        Args.b1_divisor_arg;
        Args.reassign_arg;
        Args.whitening_time_arg;
      ]
  in
  Args.parse ~allow_anon:true ~args "visualize input.wav output.png [options]";
  let anon = Args.anonymous_args () in
  if List.length anon < 2 then begin
    Printf.eprintf "Usage: %s input.wav output.png [options]\n" Sys.argv.(0);
    exit 1
  end;
  let input_file = List.nth anon 0 in
  let output_file = List.nth anon 1 in
  let dump_peaks = !dump_peaks in

  let wav = Wav.fopen input_file in

  let cqt_frames = Queue.create () in
  let all_peaks = Hashtbl.create 1024 in
  let all_pairs = Queue.create () in

  let instruments =
    {
      Audio.cqt = Some (fun frame -> Queue.push (Array.copy frame) cqt_frames);
      peaks =
        Some
          (fun peaks ->
            List.iter (fun p -> Hashtbl.replace all_peaks p ()) peaks);
      pairs =
        Some (fun pairs -> List.iter (fun p -> Queue.push p all_pairs) pairs);
    }
  in

  let merger =
    match Args.merger () with
    | Audio.Single m -> m
    | Audio.Both -> Audio.center_merger
  in
  let stream =
    Audio.hash_wav ~instruments ~merger ~params:(Args.audio_params ()) wav
  in
  let rec drain () = match stream () with Some _ -> drain () | None -> () in
  drain ();
  Wav.close wav;

  let frames = Array.of_seq (Queue.to_seq cqt_frames) in
  let n_frames = Array.length frames in
  if n_frames = 0 then (
    Printf.eprintf "No frames processed\n";
    exit 1);
  let n_bins = Array.length frames.(0) in

  let max_val =
    Array.fold_left (fun m row -> Array.fold_left Float.max m row) 0. frames
  in
  let log_max = log1p max_val in

  let img = Image.create_rgb ~alpha:false n_frames n_bins in

  for t = 0 to n_frames - 1 do
    for f = 0 to n_bins - 1 do
      let v = if log_max > 0. then log1p frames.(t).(f) /. log_max else 0. in
      let r, g, b = colormap v in
      Image.write_rgb img t (n_bins - 1 - f) r g b
    done
  done;

  Queue.iter
    (fun ((x1, y1), (x2, y2)) ->
      let py1 = n_bins - 1 - y1 and py2 = n_bins - 1 - y2 in
      draw_line img ~width:n_frames ~height:n_bins x1 py1 x2 py2 0 180 0)
    all_pairs;

  Hashtbl.iter
    (fun (x, y) () ->
      let py = n_bins - 1 - y in
      for dx = -1 to 1 do
        for dy = -1 to 1 do
          let px = x + dx and py' = py + dy in
          if px >= 0 && px < n_frames && py' >= 0 && py' < n_bins then
            Image.write_rgb img px py' 255 255 255
        done
      done)
    all_peaks;

  ImageLib_unix.writefile output_file img;

  if dump_peaks then begin
    let peaks_file = Filename.remove_extension output_file ^ "_peaks.txt" in
    let ch = open_out peaks_file in
    Hashtbl.iter (fun (x, y) () -> Printf.fprintf ch "%d %d\n" x y) all_peaks;
    close_out ch;
    Printf.eprintf "Peaks written to %s (%d peaks)\n%!" peaks_file
      (Hashtbl.length all_peaks)
  end
