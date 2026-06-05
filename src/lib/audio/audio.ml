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

type channel_merger = float array array -> float array
type merger_mode = Single of channel_merger | Both

let mono_merger chunk =
  let length = Array.length chunk.(0) in
  let chans = Array.length chunk in
  let range = Array.init chans (fun x -> x) in
  Array.init length (fun sample ->
      Array.fold_left (fun cur chan -> cur +. chunk.(chan).(sample)) 0. range
      /. float chans)

(* On stereo input, use (L-R)/2: this cancels anything panned to the centre
   (voice-overs, DJ announcements, etc.) which are typically identical in
   both channels, making the fingerprint robust to such overlays.
   On mono input, return the single channel as-is. *)
let center_merger chunk =
  let length = Array.length chunk.(0) in
  if Array.length chunk = 1 then Array.copy chunk.(0)
  else
    Array.init length (fun sample ->
        (chunk.(0).(sample) -. chunk.(1).(sample)) /. 2.0)

type audio_params = {
  samplerate : int;
  frame_step : float;
  hashes_min_freq : float;
  hashes_max_freq : float;
  hashes_bins_per_octave : float;
  hashes_reassign : bool;
  peaks_delta_x : float;
  peaks_delta_y : int;
  pairs_max_x : float;
  pairs_max_y : int;
  (* Divisor for b̂1 = ⌊b1/hashes_b1_divisor⌋ in the hash. Larger values give
     coarser hash granularity (more collisions, less discrimination). *)
  hashes_b1_divisor : int;
  (* EMA time constant (seconds) for per-bin spectral whitening before peak
     detection. 0.0 disables whitening. *)
  hashes_whitening_time : float;
}

let default_params =
  {
    samplerate = 11025;
    frame_step = 0.025;
    hashes_min_freq = 107.9;
    hashes_max_freq = 5512.5;
    hashes_bins_per_octave = 36.0;
    hashes_reassign = false;
    peaks_delta_x = 0.4;
    peaks_delta_y = 12;
    pairs_max_x = 2.;
    pairs_max_y = 35;
    hashes_b1_divisor = 6;
    hashes_whitening_time = 3.0;
  }

type 'a instrument = ('a -> unit) option

type instruments = {
  cqt : float array instrument;
  peaks : Hashes.peak list instrument;
  pairs : (Hashes.peak * Hashes.peak) list instrument;
}

let may_apply s = function
  | None -> s
  | Some f -> (
      fun () ->
        match s () with
        | Some v ->
            f v;
            Some v
        | None -> None)

let default_instruments = { cqt = None; peaks = None; pairs = None }

let hash_wav ?(instruments = default_instruments) ?(merger = mono_merger)
    ?(params = default_params) wav =
  let samplerate = float params.samplerate in
  let channels = Wav.channels wav in
  let convert, flush =
    let src_samplerate = float (Wav.sample_rate wav) in
    let final_chunk = Array.make channels [||] in
    if src_samplerate = samplerate then ((fun x -> x), fun () -> final_chunk)
    else begin
      let ratio = samplerate /. src_samplerate in
      let converters =
        Array.init channels (fun _ ->
            Samplerate.create Samplerate.Conv_linear 1)
      in
      let convert =
        Array.mapi (fun n chunk ->
            Samplerate.process_alloc converters.(n) ratio chunk 0
              (Array.length chunk))
      in
      let flush () = convert final_chunk in
      (convert, flush)
    end
  in
  let frame_step = int_of_float (params.frame_step *. samplerate) in
  let flushed = ref false in
  let chunks () =
    let buf = convert (Wav.samples wav 1024) in
    if Array.length buf.(0) <> 0 then Some buf
    else begin
      if !flushed then None
      else begin
        flushed := true;
        Some (flush ())
      end
    end
  in
  let mono_chunks =
    if channels = 1 then fun () ->
      match chunks () with Some pcm -> Some pcm.(0) | None -> None
    else fun () ->
      match chunks () with Some pcm -> Some (merger pcm) | None -> None
  in
  let cqt_params =
    {
      Fcqt.min_freq = params.hashes_min_freq;
      max_freq = params.hashes_max_freq;
      bins_per_octave = params.hashes_bins_per_octave;
      samplerate;
      step = params.frame_step;
      reassign = params.hashes_reassign;
    }
  in
  let fcqt = Fcqt.init cqt_params in
  let frames =
    Hashes.frames ~length:(Fcqt.sample_size fcqt) ~step:frame_step mono_chunks
  in
  let rec rows () =
    match frames () with
    | Some frame -> begin
        try Some (Fcqt.execute fcqt frame) with Fcqt.Need_more_data -> rows ()
      end
    | None -> None
  in
  let rows = may_apply rows instruments.cqt in
  let rows =
    if params.hashes_whitening_time <= 0. then rows
    else
      let alpha = exp (-.params.frame_step /. params.hashes_whitening_time) in
      let avg = ref [||] in
      fun () ->
        match rows () with
        | None -> None
        | Some frame ->
            if Array.length !avg = 0 then avg := Array.copy frame
            else
              Array.iteri
                (fun i v ->
                  !avg.(i) <- (alpha *. !avg.(i)) +. ((1. -. alpha) *. v))
                frame;
            Some
              (Array.mapi
                 (fun i v ->
                   let a = !avg.(i) in
                   if a < 1e-10 then 0. else v /. a)
                 frame)
  in
  let delta_x = int_of_float (params.peaks_delta_x /. params.frame_step) in
  let peaks = Hashes.peaks ~delta_x ~delta_y:params.peaks_delta_y rows in
  let peaks = may_apply peaks instruments.peaks in
  let max_x = int_of_float (params.pairs_max_x /. params.frame_step) in
  let pairs =
    Hashes.pairs ~delta_x ~delta_y:params.peaks_delta_y ~max_x
      ~max_y:params.pairs_max_y peaks
  in
  let pairs = may_apply pairs instruments.pairs in
  Hashes.hashes ~b1_divisor:params.hashes_b1_divisor pairs
