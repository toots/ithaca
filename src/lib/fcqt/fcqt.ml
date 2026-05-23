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

module FFT = Fftw3.D

let execute_fft input output elem_type plan data =
  let data = FFT.Array1.of_array elem_type Bigarray.c_layout data in
  Bigarray.Array1.blit data input;
  FFT.exec plan;
  Array.init (Bigarray.Array1.dim output) (Bigarray.Array1.get output)

let fft_c2r size =
  let input =
    FFT.Array1.create FFT.complex Bigarray.c_layout ((size / 2) + 1)
  in
  let output = FFT.Array1.create FFT.float Bigarray.c_layout size in
  let plan = FFT.Array1.c2r input output in
  execute_fft input output FFT.complex plan

let fft_r2c size =
  let input = FFT.Array1.create FFT.float Bigarray.c_layout size in
  let output =
    FFT.Array1.create FFT.complex Bigarray.c_layout ((size / 2) + 1)
  in
  let plan = FFT.Array1.r2c input output in
  execute_fft input output FFT.float plan

type params = {
  min_freq : float;
  max_freq : float;
  bins_per_octave : float;
  samplerate : float;
  step : float;
  reassign : bool;
}

type frame = Complex.t array

type t = {
  params : params;
  matrix : (int, (int * float) list) Hashtbl.t;
  t_matrix : (int, (int * float) list) Hashtbl.t;
  f_matrix : (int, (int * float) list) Hashtbl.t;
  cached_steps : int;
  deferred : float array Ringbuffer.t;
  f_of_pos : float array;
  fft : float array -> Complex.t array;
  ifft : Complex.t array -> float array;
  cqt_size : int;
  fft_size : int;
  mutable cached : int;
}

exception Need_more_data

let complex r i = { Complex.re = r; im = i }
let pi = 4.0 *. atan 1.0

let init params =
  let threshold = 0.0054 in
  let log2 x y = (log x -. log y) /. log 2.0 in
  let nextpow2 x =
    let rec fn x n = if x < n then n else fn x (2.0 *. n) in
    int_of_float (fn x 1.0)
  in
  let cexp w x = complex (w *. cos x) (w *. sin x) in
  let big_k =
    int_of_float
      (ceil (params.bins_per_octave *. log2 params.max_freq params.min_freq))
  in
  let big_q = 1.0 /. ((2.0 ** (1.0 /. params.bins_per_octave)) -. 1.0) in
  let f k = params.min_freq *. (2.0 ** (k /. params.bins_per_octave)) in
  let big_n k = ceil (big_q *. params.samplerate /. f k) in
  let n_max = nextpow2 (big_n 0.0) in
  let make_matrix window =
    let big_h k n =
      let n_k = big_n k in
      let n = float n in
      if n <= n_k then
        cexp (window n n_k /. n_k) (-2.0 *. pi *. n *. big_q /. n_k)
      else Complex.zero
    in
    let fft = fft_c2r n_max in
    let matrix = Hashtbl.create 12 in
    Array.iter
      (fun k ->
        let row = Array.init ((n_max / 2) + 1) (big_h (float k)) in
        let fft_row = fft row in
        let kernel_elems =
          List.filter
            (fun (n, v) -> threshold < abs_float v)
            (Array.to_list (Array.mapi (fun n v -> (n, v)) fft_row))
        in
        if 0 < List.length kernel_elems then Hashtbl.add matrix k kernel_elems)
      (Array.init big_k (fun x -> x));
    matrix
  in
  let hamming n len =
    (25.0 /. 46.0) -. (21.0 /. 46.0 *. cos (2.0 *. pi *. n /. len))
  in
  let t_window n len =
    (n -. (len /. 2.)) *. hamming n len /. params.samplerate
  in
  let f_window n len = if n = 0. then 0. else (-1. ** n) /. n in
  let matrix = make_matrix hamming in
  let t_matrix, f_matrix =
    if params.reassign then (make_matrix t_window, make_matrix f_window)
    else (Hashtbl.create 0, Hashtbl.create 0)
  in
  let fft = fft_r2c n_max in
  let ifft = fft_c2r n_max in
  let f_of_pos = Array.init big_k (fun k -> big_q /. big_n (float k)) in
  let cached_steps =
    int_of_float (float n_max /. (params.step *. params.samplerate))
  in
  {
    params;
    matrix;
    t_matrix;
    f_matrix;
    cached_steps;
    deferred =
      Ringbuffer.init (Array.init cached_steps (fun _ -> Array.make big_k 0.));
    f_of_pos;
    fft;
    ifft;
    cqt_size = big_k;
    fft_size = n_max;
    cached = 0;
  }

let reassign ~fcqt ~t_data ~f_data data =
  let process n d =
    let t = t_data.(n) in
    let f = f_data.(n) in
    let t_diff = (Complex.div t d).Complex.re in
    let t_pos =
      int_of_float
        ((float fcqt.cached_steps /. 2.) +. (t_diff /. fcqt.params.step))
    in
    let f_diff = (Complex.div f d).Complex.im /. (2. *. pi) in
    let f_reas = fcqt.f_of_pos.(n) -. f_diff in
    let rec f_pos direction n =
      let k, k' = if direction < 0 then (n - 1, n) else (n, n + 1) in
      if k = -1 && f_reas < fcqt.f_of_pos.(0) then -1
      else if k' = Array.length fcqt.f_of_pos then Array.length fcqt.f_of_pos
      else if fcqt.f_of_pos.(k) <= f_reas && f_reas <= fcqt.f_of_pos.(k') then
        if f_reas -. fcqt.f_of_pos.(k) < fcqt.f_of_pos.(k') -. f_reas then k
        else k'
      else f_pos direction (n + direction)
    in
    let f_pos =
      if f_reas < fcqt.f_of_pos.(n) then f_pos (-1) n else f_pos 1 n
    in
    if
      0 <= t_pos && t_pos < fcqt.cached_steps && 0 <= f_pos
      && f_pos < fcqt.cqt_size
    then begin
      let buf = Ringbuffer.get fcqt.deferred t_pos in
      buf.(f_pos) <- buf.(f_pos) +. Complex.norm2 d
    end
  in
  Array.iteri process data;
  let ret =
    Array.map (fun v -> log1p (sqrt v)) (Ringbuffer.get fcqt.deferred 0)
  in
  Ringbuffer.push fcqt.deferred (Array.make fcqt.cqt_size 0.);
  if fcqt.cached < fcqt.cached_steps / 2 then begin
    fcqt.cached <- fcqt.cached + 1;
    raise Need_more_data
  end;
  ret

let transform_matrix fcqt fft_data matrix =
  Array.init fcqt.cqt_size (fun k ->
      match Hashtbl.find_opt matrix k with
      | None -> Complex.zero
      | Some row ->
          List.fold_left
            (fun cur (n, v) ->
              let { Complex.re; im } =
                (* See: http://www.fftw.org/fftw3_doc/The-1d-Real_002ddata-DFT.html#The-1d-Real_002ddata-DFT *)
                if n <= fcqt.fft_size / 2 then fft_data.(n)
                else Complex.conj fft_data.(fcqt.fft_size - n - 1)
              in
              Complex.add cur (complex (v *. re) (v *. im)))
            Complex.zero row)

let execute_frame fcqt data =
  if Array.length data <> fcqt.fft_size then failwith "Invalid input size!";
  transform_matrix fcqt (fcqt.fft data) fcqt.matrix

let frame_magnitude frame = Array.map (fun c -> log1p (Complex.norm c)) frame

let invert_frame fcqt frame =
  let freq_buf = Array.make ((fcqt.fft_size / 2) + 1) Complex.zero in
  Array.iteri
    (fun k coeff ->
      match Hashtbl.find_opt fcqt.matrix k with
      | None -> ()
      | Some row ->
          List.iter
            (fun (n, v) ->
              let idx =
                if n <= fcqt.fft_size / 2 then n else fcqt.fft_size - n - 1
              in
              freq_buf.(idx) <-
                Complex.add freq_buf.(idx)
                  {
                    Complex.re = v *. coeff.Complex.re;
                    Complex.im = v *. coeff.Complex.im;
                  })
            row)
    frame;
  fcqt.ifft freq_buf

let execute fcqt data =
  if Array.length data <> fcqt.fft_size then failwith "Invalid input size!";
  let fft_data = fcqt.fft data in
  let data = transform_matrix fcqt fft_data fcqt.matrix in
  if fcqt.params.reassign then begin
    let t_data = transform_matrix fcqt fft_data fcqt.t_matrix in
    let f_data = transform_matrix fcqt fft_data fcqt.f_matrix in
    reassign ~fcqt ~t_data ~f_data data
  end
  else frame_magnitude data

let sample_size { fft_size } = fft_size
