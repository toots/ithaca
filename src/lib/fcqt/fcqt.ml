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

let fft_c2r size =
  let input =
    FFT.Array1.create FFT.complex Bigarray.c_layout ((size / 2) + 1)
  in
  let output = FFT.Array1.create FFT.float Bigarray.c_layout size in
  let plan = FFT.Array1.c2r input output in
  fun data ->
    let data = FFT.Array1.of_array FFT.complex Bigarray.c_layout data in
    Bigarray.Array1.blit data input;
    FFT.exec plan;
    Array.init (Bigarray.Array1.dim output) (Bigarray.Array1.get output)

(* Forward FFT filling the caller-provided [re]/[im] half-spectrum buffers,
   reused across frames to avoid per-frame allocation. *)
let fft_r2c size re im =
  let input = FFT.Array1.create FFT.float Bigarray.c_layout size in
  let output =
    FFT.Array1.create FFT.complex Bigarray.c_layout ((size / 2) + 1)
  in
  let plan = FFT.Array1.r2c input output in
  fun data ->
    for i = 0 to size - 1 do
      Bigarray.Array1.unsafe_set input i (Array.unsafe_get data i)
    done;
    FFT.exec plan;
    for i = 0 to size / 2 do
      let { Complex.re = r; im = c } = Bigarray.Array1.unsafe_get output i in
      Array.unsafe_set re i r;
      Array.unsafe_set im i c
    done

type params = {
  min_freq : float;
  max_freq : float;
  bins_per_octave : float;
  samplerate : float;
  step : float;
  reassign : bool;
}

type frame = Complex.t array

(* Sparse kernel row: [k_idx] holds half-spectrum indices (spectrum indices
   above fft_size/2 are already resolved to their mirrored position), [k_re]
   the coefficient and [k_im] the coefficient with the conjugation sign
   folded in. *)
type kernel = { k_idx : int array; k_re : float array; k_im : float array }

type t = {
  params : params;
  matrix : kernel array;
  t_matrix : kernel array;
  f_matrix : kernel array;
  cached_steps : int;
  mutable deferred : float array Ringbuffer.t;
  f_of_pos : float array;
  fft : float array -> unit;
  fft_re : float array;
  fft_im : float array;
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
    Array.init big_k (fun k ->
        let row = Array.init ((n_max / 2) + 1) (big_h (float k)) in
        let fft_row = fft row in
        let kernel_elems =
          List.filter
            (fun (_, v) -> threshold < abs_float v)
            (Array.to_list (Array.mapi (fun n v -> (n, v)) fft_row))
        in
        let count = List.length kernel_elems in
        let k_idx = Array.make count 0 in
        let k_re = Array.make count 0. in
        let k_im = Array.make count 0. in
        List.iteri
          (fun j (n, v) ->
            let conj = n > n_max / 2 in
            k_idx.(j) <- (if conj then n_max - n - 1 else n);
            k_re.(j) <- v;
            k_im.(j) <- (if conj then -.v else v))
          kernel_elems;
        { k_idx; k_re; k_im })
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
    else ([||], [||])
  in
  let fft_re = Array.make ((n_max / 2) + 1) 0. in
  let fft_im = Array.make ((n_max / 2) + 1) 0. in
  let fft = fft_r2c n_max fft_re fft_im in
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
    fft_re;
    fft_im;
    ifft;
    cqt_size = big_k;
    fft_size = n_max;
    cached = 0;
  }

(* Clear the per-file transient state so the processor (plans + kernels) can be
   reused for the next file without rebuilding. *)
let reset t =
  t.cached <- 0;
  t.deferred <-
    Ringbuffer.init
      (Array.init t.cached_steps (fun _ -> Array.make t.cqt_size 0.))

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

(* Apply a sparse kernel matrix to the current content of the shared
   [fft_re]/[fft_im] buffers. *)
let transform_matrix fcqt matrix =
  let fft_re = fcqt.fft_re in
  let fft_im = fcqt.fft_im in
  Array.init fcqt.cqt_size (fun k ->
      let { k_idx; k_re; k_im } = matrix.(k) in
      let re = ref 0. in
      let im = ref 0. in
      for j = 0 to Array.length k_idx - 1 do
        let m = Array.unsafe_get k_idx j in
        re := !re +. (Array.unsafe_get k_re j *. Array.unsafe_get fft_re m);
        im := !im +. (Array.unsafe_get k_im j *. Array.unsafe_get fft_im m)
      done;
      complex !re !im)

let execute_frame fcqt data =
  if Array.length data <> fcqt.fft_size then failwith "Invalid input size!";
  fcqt.fft data;
  transform_matrix fcqt fcqt.matrix

let frame_magnitude frame = Array.map (fun c -> log1p (Complex.norm c)) frame

let invert_frame fcqt frame =
  let freq_buf = Array.make ((fcqt.fft_size / 2) + 1) Complex.zero in
  Array.iteri
    (fun k coeff ->
      let { k_idx; k_re; _ } = fcqt.matrix.(k) in
      Array.iteri
        (fun j idx ->
          let v = k_re.(j) in
          freq_buf.(idx) <-
            Complex.add freq_buf.(idx)
              {
                Complex.re = v *. coeff.Complex.re;
                Complex.im = v *. coeff.Complex.im;
              })
        k_idx)
    frame;
  fcqt.ifft freq_buf

let execute fcqt data =
  if Array.length data <> fcqt.fft_size then failwith "Invalid input size!";
  fcqt.fft data;
  let data = transform_matrix fcqt fcqt.matrix in
  if fcqt.params.reassign then begin
    let t_data = transform_matrix fcqt fcqt.t_matrix in
    let f_data = transform_matrix fcqt fcqt.f_matrix in
    reassign ~fcqt ~t_data ~f_data data
  end
  else frame_magnitude data

let sample_size { fft_size; _ } = fft_size
