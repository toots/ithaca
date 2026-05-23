open OUnit2

let suite =
  [
    ( "Float buffer" >:: fun _ ->
      let buf = Float_buffer.init () in
      Float_buffer.add buf [| 1.0; 2.0; 3.0 |];
      Float_buffer.add buf [| 4.0; 5.0 |];
      Float_buffer.add buf [| 6.0; 7.0; 8.0; 9.0 |];
      assert_equal 9 (Float_buffer.length buf);
      assert_equal [| 1.0; 2.0; 3.0; 4.0 |] (Float_buffer.peek buf 4);
      assert_equal 4 (Float_buffer.drop buf 4);
      assert_equal 5 (Float_buffer.length buf);
      assert_equal [| 5.0; 6.0; 7.0 |] (Float_buffer.peek buf 3);
      assert_equal [| 5.0; 6.0; 7.0; 8.0; 9.0 |] (Float_buffer.peek buf 12);
      assert_equal 5 (Float_buffer.drop buf 23) );
  ]
