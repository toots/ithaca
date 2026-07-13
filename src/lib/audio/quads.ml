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
let max_interior = 4

(* Maximum quads emitted per incoming peak, to bound hash density. *)
let max_quads_per_peak = 32

(* Quantization: [bits] per component, so a cell is 1/2^bits of the box
   span. Coarse on purpose: whitening-state differences between the query
   clip and the indexed track shift peaks by a bin or so, which moves a
   normalized component by roughly 1/box-span. *)
let bits = 5
let cells_per_axis = 1 lsl bits

(* Query-side tolerance: probe every cell intersecting [v - j, v + j]. *)
let jitter = 0.04

let quantize v =
  let q = int_of_float (v *. float cells_per_axis) in
  if q < 0 then 0 else if q > cells_per_axis - 1 then cells_per_axis - 1 else q

let pack q1 q2 q3 q4 =
  (((((q1 lsl bits) lor q2) lsl bits) lor q3) lsl bits) lor q4

let normalize (ax, ay) (bx, by) (px, py) =
  (float (px - ax) /. float (bx - ax), float (py - ay) /. float (by - ay))

let hash a b c d =
  let cx, cy = normalize a b c in
  let dx, dy = normalize a b d in
  pack (quantize cx) (quantize cy) (quantize dx) (quantize dy)

(* All quantization cells a component may fall in on the reference side,
   given the jitter allowance. *)
let cells v =
  let lo = quantize (v -. jitter) in
  let hi = quantize (v +. jitter) in
  List.init (hi - lo + 1) (fun i -> lo + i)

(* Hashes for all jitter-tolerant cell combinations. *)
let probe_hashes a b c d =
  let cx, cy = normalize a b c in
  let dx, dy = normalize a b d in
  List.concat_map
    (fun q1 ->
      List.concat_map
        (fun q2 ->
          List.concat_map
            (fun q3 -> List.map (fun q4 -> pack q1 q2 q3 q4) (cells dy))
            (cells dx))
        (cells cy))
    (cells cx)

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

let hashes ?(probes = false) ~max_x ~max_y peaks =
  let buffer = ref [] in
  let queue = Queue.create () in
  let min_dx = max 1 (max_x / min_dx_divisor) in
  let emit ((ax, ay) as a) b c d =
    let hashes = if probes then probe_hashes a b c d else [ hash a b c d ] in
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
