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

type 'a t = unit -> 'a option

let make l =
  let s = ref l in
  fun () ->
    match !s with
    | [] -> None
    | x :: rest ->
        s := rest;
        Some x

(* Invert a push-based producer into a pull stream: [iter] runs as an
   effect-handled coroutine, suspended at each element it yields and resumed by
   the next pull. The producer only runs to completion (and releases its
   resources) if the stream is pulled until [None]. *)
let of_iter (type elt) (iter : (elt -> unit) -> unit) : elt t =
  let open Effect in
  let open Effect.Deep in
  let module Gen = struct
    type _ Effect.t += Yield : elt -> unit Effect.t
  end in
  let next = ref (fun () -> None) in
  let start () =
    match_with iter
      (fun elt -> perform (Gen.Yield elt))
      {
        retc =
          (fun () ->
            (next := fun () -> None);
            None);
        exnc = raise;
        effc =
          (fun (type resume) (eff : resume Effect.t) ->
            match eff with
            | Gen.Yield elt ->
                Some
                  (fun (k : (resume, elt option) continuation) ->
                    (next := fun () -> continue k ());
                    Some elt)
            | _ -> None);
      }
  in
  next := start;
  fun () -> !next ()

let pull s =
  let ret = ref [] in
  let rec pull () =
    match s () with
    | Some x ->
        ret := x :: !ret;
        pull ()
    | None -> ()
  in
  pull ();
  List.rev !ret
