open OUnit2

(* A small constellation: anchor (0, 10), far corner (30, 40), and two
   interior peaks. Emitted row by row like the peaks stream does. *)
let constellation dy =
  [ [ (0, 10 + dy) ]; [ (8, 20 + dy) ]; [ (15, 30 + dy) ]; [ (30, 40 + dy) ] ]

let pull_hashes ?probes rows =
  IStream.pull (Quads.hashes ?probes ~max_x:40 ~max_y:64 (IStream.make rows))

let suite =
  [
    ( "Quad hash is invariant under pitch translation" >:: fun _ ->
      let reference = pull_hashes (constellation 0) in
      let shifted = pull_hashes (constellation 7) in
      assert_bool "some quads were emitted" (0 < List.length reference);
      assert_equal
        (List.map (fun { Hashes.hash; _ } -> hash) reference)
        (List.map (fun { Hashes.hash; _ } -> hash) shifted);
      (* The anchor bin follows the translation: this is what carries the
         pitch offset. *)
      List.iter2
        (fun r s -> assert_equal (r.Hashes.bin + 7) s.Hashes.bin)
        reference shifted );
    ( "Quad hash discriminates interior geometry" >:: fun _ ->
      let reference = pull_hashes (constellation 0) in
      let different =
        pull_hashes [ [ (0, 10) ]; [ (8, 36) ]; [ (15, 12) ]; [ (30, 40) ] ]
      in
      assert_bool "different geometry, different hashes"
        (List.map (fun { Hashes.hash; _ } -> hash) reference
        <> List.map (fun { Hashes.hash; _ } -> hash) different) );
    ( "Probes include the exact hash" >:: fun _ ->
      let exact =
        List.map
          (fun { Hashes.hash; _ } -> hash)
          (pull_hashes (constellation 0))
      in
      let probed =
        List.map
          (fun { Hashes.hash; _ } -> hash)
          (pull_hashes ~probes:true (constellation 0))
      in
      assert_bool "at least as many probe hashes"
        (List.length exact <= List.length probed);
      List.iter
        (fun h -> assert_bool "exact hash present" (List.mem h probed))
        exact );
    ( "Anchor position and bin are the anchor peak's" >:: fun _ ->
      match pull_hashes (constellation 0) with
      | { Hashes.pos; bin; _ } :: _ ->
          assert_equal 0 pos;
          assert_equal 10 bin
      | [] -> assert_failure "no quads emitted" );
  ]
