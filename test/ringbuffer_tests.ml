open OUnit2

let suite =
  [
    ( "Ringbuffer" >:: fun _ ->
      let buf = Ringbuffer.init [| 1; 2; 3; 4; 5; 6 |] in
      assert_equal (Ringbuffer.get buf 0) 1;
      assert_equal (Ringbuffer.get buf 2) 3;
      Ringbuffer.push buf 7;
      assert_equal (Ringbuffer.get buf 0) 2;
      assert_equal (Ringbuffer.get buf 2) 4;
      Ringbuffer.push buf 8;
      assert_equal (Ringbuffer.get buf 0) 3;
      assert_equal (Ringbuffer.get buf 2) 5;
      Ringbuffer.push buf 9;
      assert_equal (Ringbuffer.get buf 0) 4;
      assert_equal (Ringbuffer.get buf 2) 6;
      Ringbuffer.push buf 10;
      assert_equal (Ringbuffer.get buf 0) 5;
      assert_equal (Ringbuffer.get buf 2) 7;
      Ringbuffer.push buf 11;
      assert_equal (Ringbuffer.get buf 0) 6;
      assert_equal (Ringbuffer.get buf 2) 8;
      Ringbuffer.push buf 12;
      assert_equal (Ringbuffer.get buf 0) 7;
      assert_equal (Ringbuffer.get buf 2) 9;
      Ringbuffer.push buf 13;
      assert_equal (Ringbuffer.get buf 0) 8;
      assert_equal (Ringbuffer.get buf 2) 10;
      Ringbuffer.push buf 14;
      assert_equal (Ringbuffer.get buf 0) 9;
      assert_equal (Ringbuffer.get buf 2) 11 );
  ]
