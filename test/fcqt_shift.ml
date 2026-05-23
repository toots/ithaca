let write_i16 out n =
  output_char out (Char.chr (n land 0xff));
  output_char out (Char.chr ((n lsr 8) land 0xff))

let write_i32 out n =
  output_char out (Char.chr (n land 0xff));
  output_char out (Char.chr ((n lsr 8) land 0xff));
  output_char out (Char.chr ((n lsr 16) land 0xff));
  output_char out (Char.chr ((n lsr 24) land 0xff))

let write_wav_header out ~channels ~sample_rate ~data_size =
  output_string out "RIFF";
  write_i32 out (36 + data_size);
  output_string out "WAVEfmt ";
  write_i32 out 16;
  write_i16 out 1;
  write_i16 out channels;
  write_i32 out sample_rate;
  write_i32 out (sample_rate * channels * 2);
  write_i16 out (channels * 2);
  write_i16 out 16;
  output_string out "data";
  write_i32 out data_size

let patch_wav_sizes out ~channels n_samples =
  let data_size = n_samples * channels * 2 in
  seek_out out 4;
  write_i32 out (36 + data_size);
  seek_out out 40;
  write_i32 out data_size

let write_sample out s =
  let v = int_of_float (Float.round (s *. 32767.)) in
  let v = max (-32768) (min 32767 v) in
  write_i16 out (v land 0xffff)

let shift_frame shift_bins frame =
  let n = Array.length frame in
  Array.init n (fun k ->
      let src = k - shift_bins in
      if src >= 0 && src < n then frame.(src) else Complex.zero)

let () =
  if Array.length Sys.argv <> 4 then begin
    Printf.eprintf "Usage: %s input.wav semitones output.wav\n" Sys.argv.(0);
    exit 1
  end;
  let input_file = Sys.argv.(1) in
  let semitones = float_of_string Sys.argv.(2) in
  let output_file = Sys.argv.(3) in

  let wav = Wav.fopen input_file in
  let samplerate = Wav.sample_rate wav in
  let samplerate_f = float samplerate in
  let channels = Wav.channels wav in

  let bins_per_octave = 36.0 in
  let min_freq = 55.0 in
  let max_freq = samplerate_f *. 0.45 in
  let frame_step = 0.025 in
  let cqt_params =
    {
      Fcqt.min_freq;
      max_freq;
      bins_per_octave;
      samplerate = samplerate_f;
      step = frame_step;
      reassign = false;
    }
  in
  let fcqt = Fcqt.init cqt_params in
  let fft_size = Fcqt.sample_size fcqt in
  let step_size = int_of_float (frame_step *. samplerate_f) in
  let shift_bins =
    int_of_float (Float.round (semitones *. bins_per_octave /. 12.0))
  in
  let norm = float fft_size in

  Printf.eprintf
    "Sample rate: %d Hz, channels: %d, fft_size: %d, step: %d, shift: %d bins\n\
     %!"
    samplerate channels fft_size step_size shift_bins;

  (* Estimate total frames for progress bar *)
  let wav_header = Wav.header wav in
  let n_wav_frames = Wav.data_length wav / wav_header.bytes_per_sample in
  let n_total_frames = max 1 (((n_wav_frames - fft_size) / step_size) + 1) in
  let bar_width = 40 in
  let last_bar = ref (-1) in
  let print_progress frame =
    let filled = min bar_width (frame * bar_width / n_total_frames) in
    if filled <> !last_bar then begin
      last_bar := filled;
      Printf.eprintf "\r[%s%s] %d%%" (String.make filled '#')
        (String.make (bar_width - filled) '.')
        (min 100 (frame * 100 / n_total_frames));
      flush stderr
    end
  in

  (* One Float_buffer per channel *)
  let input_bufs = Array.init channels (fun _ -> Float_buffer.init ()) in
  let wav_done = ref false in

  let fill_input () =
    if not !wav_done then begin
      let pcm = Wav.samples wav 1024 in
      if Array.length pcm.(0) = 0 then wav_done := true
      else Array.iteri (fun ch buf -> Float_buffer.add buf pcm.(ch)) input_bufs
    end
  in
  let ensure_input n =
    while Float_buffer.length input_bufs.(0) < n && not !wav_done do
      fill_input ()
    done
  in

  (* One sliding overlap-add window per channel *)
  let out_bufs = Array.init channels (fun _ -> Array.make fft_size 0.) in
  let weight_buf = Array.make fft_size 0. in

  let out_channel = open_out_bin output_file in
  write_wav_header out_channel ~channels ~sample_rate:samplerate ~data_size:0;
  let n_written = ref 0 in

  let flush_step () =
    for i = 0 to step_size - 1 do
      Array.iter
        (fun ob ->
          let v =
            if weight_buf.(i) > 0. then ob.(i) /. (weight_buf.(i) *. norm)
            else 0.
          in
          write_sample out_channel v)
        out_bufs
    done;
    n_written := !n_written + step_size;
    Array.iter
      (fun ob ->
        Array.blit ob step_size ob 0 (fft_size - step_size);
        Array.fill ob (fft_size - step_size) step_size 0.)
      out_bufs;
    Array.blit weight_buf step_size weight_buf 0 (fft_size - step_size);
    Array.fill weight_buf (fft_size - step_size) step_size 0.
  in

  let process_frame () =
    Array.iteri
      (fun ch ob ->
        let frame = Float_buffer.peek input_bufs.(ch) fft_size in
        let cqt_frame = Fcqt.execute_frame fcqt frame in
        let shifted = shift_frame shift_bins cqt_frame in
        let reconstructed = Fcqt.invert_frame fcqt shifted in
        for i = 0 to fft_size - 1 do
          ob.(i) <- ob.(i) +. reconstructed.(i);
          if ch = 0 then weight_buf.(i) <- weight_buf.(i) +. 1.
        done)
      out_bufs;
    flush_step ()
  in

  let frame_count = ref 0 in
  ensure_input fft_size;
  while Float_buffer.length input_bufs.(0) >= fft_size do
    print_progress !frame_count;
    process_frame ();
    Array.iter (fun buf -> ignore (Float_buffer.drop buf step_size)) input_bufs;
    incr frame_count;
    ensure_input fft_size
  done;

  let remaining = fft_size / step_size in
  for _ = 1 to remaining do
    flush_step ()
  done;

  Printf.eprintf "\r[%s] 100%%\n" (String.make bar_width '#');
  Wav.close wav;
  patch_wav_sizes out_channel ~channels !n_written;
  close_out out_channel
