open OUnit2

let suite =
  [
    ( "Frames stream" >:: fun _ ->
      let chunks =
        IStream.make
          [ [| 1.0; 2.0; 3.0 |]; [| 4.0; 5.0 |]; [| 6.0; 7.0; 8.0; 9.0 |] ]
      in
      let frames = Hashes.frames ~length:4 ~step:2 chunks in
      assert_equal (Some [| 1.0; 2.0; 3.0; 4.0 |]) (frames ());
      assert_equal (Some [| 3.0; 4.0; 5.0; 6.0 |]) (frames ());
      assert_equal (Some [| 5.0; 6.0; 7.0; 8.0 |]) (frames ());
      assert_equal (Some [| 7.0; 8.0; 9.0; 0.0 |]) (frames ());
      assert_equal (Some [| 9.0; 0.0; 0.0; 0.0 |]) (frames ());
      assert_equal None (frames ());
      assert_equal None (frames ()) );
    ( "Peaks stream" >:: fun _ ->
      let rows =
        IStream.make
          [
            [| 1.0; 4.0; 2.0; 0.0 |];
            [| 3.0; 1.0; 1.0; 2.0 |];
            [| 1.0; 2.0; 3.0; 1.0 |];
            [| 2.0; 1.0; 1.0; 2.0 |];
            [| 1.0; 2.0; 1.0; 4.0 |];
          ]
      in
      let peaks = Hashes.peaks ~delta_x:1 ~delta_y:1 rows in
      assert_equal (Some [ (0, 1) ]) (peaks ());
      assert_equal (Some []) (peaks ());
      assert_equal (Some [ (2, 2) ]) (peaks ());
      assert_equal (Some []) (peaks ());
      assert_equal (Some [ (4, 3) ]) (peaks ());
      assert_equal None (peaks ());
      assert_equal None (peaks ()) );
    ( "Pairs stream" >:: fun _ ->
      let peaks =
        IStream.make
          [
            [ (0, 1); (0, 2) ];
            [];
            [ (2, 3); (2, 8) ];
            [];
            [];
            [ (5, 2); (5, 5) ];
          ]
      in
      let pairs = Hashes.pairs ~delta_x:2 ~delta_y:2 ~max_x:3 ~max_y:2 peaks in
      assert_equal (Some []) (pairs ());
      assert_equal (Some []) (pairs ());
      assert_equal (Some [ ((0, 1), (2, 3)); ((0, 2), (2, 3)) ]) (pairs ());
      assert_equal (Some []) (pairs ());
      assert_equal (Some []) (pairs ());
      assert_equal (Some [ ((2, 3), (5, 2)); ((2, 3), (5, 5)) ]) (pairs ());
      assert_equal None (pairs ());
      assert_equal None (pairs ()) );
    ( "Hash function" >:: fun _ ->
      let h = Hashes.hash (2, 3) (4, 5) in
      (* Time-translation invariant: same b1_hat and same deltas → same hash *)
      assert_equal h (Hashes.hash (10, 3) (12, 5));
      (* Different differences → different hash *)
      assert_bool "distinct hashes differ" (h <> Hashes.hash (2, 3) (5, 6)) );
    ( "Hashes stream" >:: fun _ ->
      let pairs =
        IStream.make
          [
            [ ((2, 3), (4, 5)); ((4, 2), (3, 1)) ];
            [];
            [];
            [ ((0, 4), (1, 3)); ((0, 1), (1, 3)); ((1, 3), (4, 2)) ];
          ]
      in
      let hashes = Hashes.hashes pairs in
      let check_entry pos bin hash =
        assert_equal (Some { Hashes.pos; hash; bin })
      in
      let h0 = Hashes.hash (2, 3) (4, 5) in
      let h1 = Hashes.hash (4, 2) (3, 1) in
      let h2 = Hashes.hash (0, 4) (1, 3) in
      let h3 = Hashes.hash (0, 1) (1, 3) in
      let h4 = Hashes.hash (1, 3) (4, 2) in
      check_entry 2 3 h0 (hashes ());
      check_entry 4 2 h1 (hashes ());
      check_entry 0 4 h2 (hashes ());
      check_entry 0 1 h3 (hashes ());
      check_entry 1 3 h4 (hashes ());
      assert_equal None (hashes ());
      assert_equal None (hashes ()) );
  ]
