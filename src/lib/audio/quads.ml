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

(* Quad-based descriptors after Sonnleitner & Widmer, "Robust Quad-Based
   Audio Fingerprinting" (IEEE TASLP 2016).

   A quad is four spectral peaks: an anchor A, a far corner B, and two
   peaks C, D strictly inside the box spanned by A and B. C and D are
   normalized into the unit square defined by A and B, which cancels any
   translation or scaling of the peak constellation. On a log-frequency
   (CQT) axis a pitch shift is a pure translation, so the descriptor is
   invariant to pitch shifting of any magnitude; the shift itself is
   recovered separately from the stored anchor bin.

   The four normalized coordinates are quantized to 8 bits each and packed
   into the same 32-bit hash space used by the pair scheme, so storage and
   search are unchanged. Quantization-boundary tolerance is handled at
   query time by also probing adjacent cells (see [probes]). *)

(* Minimum box width as a fraction of max_x, and minimum box height in
   bins: very small boxes make the normalized coordinates unstable, since
   integer peak jitter is divided by the box span. *)
let min_dx_divisor = 8
let min_dy = 4

(* Number of interior peaks considered per (A, B) box: all C(k, 2) pairs
   are emitted so a single missing peak on the query side does not lose
   every quad of the box. *)
let max_interior = 3

(* Maximum quads emitted per incoming peak defaults to this but is a caller
   parameter (see [hashes]): it dominates both database size and per-frame
   search cost, so it is the primary knob for trading recall against size.
   Recall comes from many boxes each contributing a few quads, not from one
   box contributing many. *)
let default_quads_per_peak = 6

(* Quantization: [bits] per normalized component, so a cell is 1/2^bits of
   the box span. Real pitch shifts (via a resampling shifter) move a
   normalized coordinate by ~10% of the box span, not the ~0% a clean
   log-frequency translation would, so cells are deliberately coarse: finer
   cells simply never match across a shift. *)
let bits = 4
let cells_per_axis = 1 lsl bits

(* Query-side tolerance, as a fraction of a cell: a component within this
   much of a cell boundary also probes the neighbouring cell (see
   [probe_hashes]). *)
let jitter = 0.5

let quantize v =
  let q = int_of_float (v *. float cells_per_axis) in
  if q < 0 then 0 else if q > cells_per_axis - 1 then cells_per_axis - 1 else q

(* The box dimensions — width in frames, height in bins — are also part of
   the key. Both are pitch invariant (a pitch shift translates every bin
   equally, leaving bin differences unchanged, and does not move peaks in
   time), so folding them in multiplies the key space without breaking
   invariance. This keeps buckets sparse at scale, where the 4 interior
   coordinates alone (16 bits) collide across unrelated tracks. Height is
   quantised coarsely because peak jitter shifts it by a bin or two; width
   is stable under pitch shift and quantised finer. *)
let dx_bits = 4
let dy_bits = 3

let box_band v ~lo ~hi ~bits =
  let n = 1 lsl bits in
  let q = int_of_float (float (v - lo) /. float (max 1 (hi - lo)) *. float n) in
  if q < 0 then 0 else if q >= n then n - 1 else q

let box_bands ~max_x ~max_y (ax, ay) (bx, by) =
  let min_dx = max 1 (max_x / min_dx_divisor) in
  ( box_band (bx - ax) ~lo:min_dx ~hi:max_x ~bits:dx_bits,
    box_band (abs (by - ay)) ~lo:min_dy ~hi:max_y ~bits:dy_bits )

let pack ~dx_band ~dy_band q1 q2 q3 q4 =
  let interior = (((((q1 lsl bits) lor q2) lsl bits) lor q3) lsl bits) lor q4 in
  interior lor (dx_band lsl 16) lor (dy_band lsl (16 + dx_bits))

let normalize (ax, ay) (bx, by) (px, py) =
  (float (px - ax) /. float (bx - ax), float (py - ay) /. float (by - ay))

let hash ~max_x ~max_y a b c d =
  let cx, cy = normalize a b c in
  let dx, dy = normalize a b d in
  let dx_band, dy_band = box_bands ~max_x ~max_y a b in
  pack ~dx_band ~dy_band (quantize cx) (quantize cy) (quantize dx) (quantize dy)

(* The cell a component falls in, and the adjacent cell if it lies within
   [jitter] of a boundary (else [None]). *)
let axis_cell v =
  let q = quantize v in
  let frac = (v *. float cells_per_axis) -. floor (v *. float cells_per_axis) in
  if frac < jitter && q > 0 then (q, Some (q - 1))
  else if frac > 1. -. jitter && q < cells_per_axis - 1 then (q, Some (q + 1))
  else (q, None)

(* Query-side hashes: the exact cell plus, for each axis near a boundary,
   the neighbour with only that axis shifted (Hamming-1 neighbourhood).
   Bounded to at most 1 + 4 hashes per quad, versus the combinatorial
   product of per-axis neighbours. Genuine matches drift on one axis at a
   time far more often than on several at once, so this recovers most of
   the recall of full neighbour probing at a fraction of the fan-out. *)
let probe_hashes ~max_x ~max_y a b c d =
  let cx, cy = normalize a b c in
  let dx, dy = normalize a b d in
  let dx_band, dy_band = box_bands ~max_x ~max_y a b in
  let pack = pack ~dx_band ~dy_band in
  let m0, n0 = axis_cell cx in
  let m1, n1 = axis_cell cy in
  let m2, n2 = axis_cell dx in
  let m3, n3 = axis_cell dy in
  let acc = [ pack m0 m1 m2 m3 ] in
  let acc = match n0 with Some n -> pack n m1 m2 m3 :: acc | None -> acc in
  let acc = match n1 with Some n -> pack m0 n m2 m3 :: acc | None -> acc in
  let acc = match n2 with Some n -> pack m0 m1 n m3 :: acc | None -> acc in
  match n3 with Some n -> pack m0 m1 m2 n :: acc | None -> acc

let interior_pairs peaks =
  let rec take n = function
    | [] -> []
    | _ when n = 0 -> []
    | p :: rest -> p :: take (n - 1) rest
  in
  let peaks = take max_interior (List.sort compare peaks) in
  let rec pairs = function
    | [] -> []
    | c :: rest -> List.map (fun d -> (c, d)) rest @ pairs rest
  in
  pairs peaks

let hashes ?(probes = false) ?(max_quads_per_peak = default_quads_per_peak)
    ~max_x ~max_y peaks =
  let buffer = ref [] in
  let queue = Queue.create () in
  let min_dx = max 1 (max_x / min_dx_divisor) in
  let emit ((ax, ay) as a) b c d =
    let hashes =
      if probes then probe_hashes ~max_x ~max_y a b c d
      else [ hash ~max_x ~max_y a b c d ]
    in
    List.iter
      (fun hash -> Queue.push { Hashes.pos = ax; hash; bin = ay } queue)
      hashes
  in
  let process_peak ((bx, by) as b) =
    let emitted = ref 0 in
    List.iter
      (fun ((ax, ay) as a) ->
        if
          !emitted < max_quads_per_peak
          && min_dx <= bx - ax
          && bx - ax <= max_x
          && min_dy <= abs (by - ay)
          && abs (by - ay) <= max_y
        then begin
          let interior =
            List.filter
              (fun (px, py) ->
                ax < px && px < bx
                &&
                let fy = float (py - ay) /. float (by - ay) in
                0. < fy && fy < 1.)
              !buffer
          in
          List.iter
            (fun (c, d) ->
              if !emitted < max_quads_per_peak then begin
                emit a b c d;
                incr emitted
              end)
            (interior_pairs interior)
        end)
      (List.sort compare !buffer)
  in
  let process_row row =
    List.iter process_peak row;
    buffer := row @ !buffer;
    match row with
    | (x, _) :: _ ->
        buffer := List.filter (fun (px, _) -> x - px <= max_x) !buffer
    | [] -> ()
  in
  fun () ->
    try Some (Queue.take queue)
    with Queue.Empty ->
      let rec pull () =
        match peaks () with
        | Some [] -> pull ()
        | Some row -> begin
            process_row row;
            match Queue.take_opt queue with Some h -> Some h | None -> pull ()
          end
        | None -> None
      in
      pull ()
