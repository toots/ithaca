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

type peak = int * int
type hash = Int32.t
type hashes = { pos : int; hash : hash }
type t = hashes IStream.t

module HashSet = Set.Make (struct
  type t = hash

  let compare = compare
end)

let frames ~length ~step chunks =
  let buffer = Float_buffer.init () in
  let rec fill len =
    if len < Float_buffer.length buffer then ()
    else
      match chunks () with
      | Some data ->
          Float_buffer.add buffer data;
          fill len
      | None -> ()
  in
  let frames () =
    fill length;
    if Float_buffer.length buffer = 0 then None
    else begin
      let frame =
        if Float_buffer.length buffer < length then begin
          let frame = Array.make length 0. in
          let rem = Float_buffer.peek buffer length in
          Array.blit rem 0 frame 0 (Array.length rem);
          frame
        end
        else Float_buffer.peek buffer length
      in
      fill step;
      ignore (Float_buffer.drop buffer step);
      Some frame
    end
  in
  frames

exception Not_a_peak

(* We find peaks by keeping a ringbuffer of 2*delta_x rows
   and looking for peaks at row delta_x, flushing with
   blank rows when necessary. *)
let peaks ~delta_x ~delta_y rows =
  let rec initial_grab cur =
    if delta_x < List.length cur then Array.of_list (List.rev cur)
    else
      match rows () with
      | Some row -> initial_grab (row :: cur)
      | None -> raise Not_found
  in
  let initial_values = initial_grab [] in
  let height = Array.length initial_values.(0) in
  let blank_row = Array.make height 0.0 in
  let initial_values =
    Array.concat [ Array.make delta_x blank_row; initial_values ]
  in
  let buffer = Ringbuffer.init initial_values in
  let value_at x y = (Ringbuffer.get buffer x).(y) in
  let get_peaks () =
    let rec find_peaks y peaks =
      if y = height then List.rev peaks
      else
        let min_y = max 0 (y - delta_y) in
        let max_y = min (height - 1) (y + delta_y) in
        try
          let cur_val = value_at delta_x y in
          if cur_val = 0.0 then raise Not_a_peak;
          for cur_x = 0 to 2 * delta_x do
            for cur_y = min_y to max_y do
              if
                (cur_x != delta_x || cur_y != y)
                && cur_val <= value_at cur_x cur_y
              then raise Not_a_peak
            done
          done;
          find_peaks (max_y + 1) (y :: peaks)
        with Not_a_peak -> find_peaks (y + 1) peaks
    in
    find_peaks 0 []
  in
  let position = ref 0 in
  let to_flush = ref delta_x in
  fun () ->
    let fill () =
      begin match rows () with
      | Some row -> Ringbuffer.push buffer row
      | None ->
          Ringbuffer.push buffer blank_row;
          decr to_flush
      end;
      incr position
    in
    if !to_flush < 0 then None
    else begin
      let peaks = List.map (fun y -> (!position, y)) (get_peaks ()) in
      fill ();
      Some peaks
    end

let pairs ~delta_x ~delta_y ~max_x ~max_y peaks =
  let buffer = ref [] in
  let find_peaks peaks =
    let max_cur_x = ref 0 in
    let pairs =
      List.fold_left
        (fun cur (x, y) ->
          let ret =
            List.fold_left
              (fun cur (x', y') ->
                if
                  (x <> x' || y <> y')
                  && (delta_x <= abs (x - x') || delta_y <= abs (y - y'))
                  && abs (x - x') <= max_x
                  && abs (y - y') <= max_y
                then ((x', y'), (x, y)) :: cur
                else cur)
              [] !buffer
          in
          if !max_cur_x < x then max_cur_x := x;
          buffer := (x, y) :: !buffer;
          cur @ ret)
        [] peaks
    in
    buffer := List.filter (fun (x, _) -> abs (!max_cur_x - x) <= max_x) !buffer;
    pairs
  in
  fun () ->
    match peaks () with
    | Some [] -> Some []
    | Some peaks -> Some (find_peaks peaks)
    | None -> None

let hash ?(b1_divisor = 6) (x1, y1) (x2, y2) =
  let b1_hat = y1 / b1_divisor in
  let str = Printf.sprintf "%i%i%i" b1_hat (y2 - y1) (x2 - x1) in
  let digest = Cryptokit.hash_string (Cryptokit.Hash.sha256 ()) str in
  Int32.of_string
    (Printf.sprintf "0x%x%x%x%x"
       (Char.code digest.[0])
       (Char.code digest.[1])
       (Char.code digest.[2])
       (Char.code digest.[3]))

let merge h1 h2 =
  let next1 = ref (h1 ()) in
  let next2 = ref (h2 ()) in
  fun () ->
    match (!next1, !next2) with
    | None, None -> None
    | Some h, None ->
        next1 := h1 ();
        Some h
    | None, Some h ->
        next2 := h2 ();
        Some h
    | Some h1_h, Some h2_h ->
        if h1_h.pos <= h2_h.pos then begin
          next1 := h1 ();
          Some h1_h
        end
        else begin
          next2 := h2 ();
          Some h2_h
        end

let hashes ?(b1_divisor = 6) pairs =
  let queue = Queue.create () in
  fun () ->
    try Some (Queue.take queue)
    with Queue.Empty ->
      let rec pull () =
        match pairs () with
        | Some [] -> pull ()
        | Some l ->
            List.iter
              (fun (anchor, target) ->
                Queue.push
                  { pos = fst anchor; hash = hash ~b1_divisor anchor target }
                  queue)
              l;
            Some (Queue.take queue)
        | None -> None
      in
      pull ()
