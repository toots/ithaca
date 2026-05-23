open OUnit2

let suite =
  [
    ( "Test best match" >:: fun _ ->
      let search_called = ref false in
      let search hashes =
        search_called := true;
        assert_equal [ 34l; 78l ] hashes;
        [
          [ { Db.id = 1; pos = 12 }; { Db.id = 3; pos = 43 } ];
          [ { Db.id = 1; pos = 56 } ];
        ]
      in
      let search_map = Search_map.init search in
      let positions = Hashtbl.create 2 in
      Hashtbl.add positions 34l { Search_map.rel_pos = 2 };
      Hashtbl.add positions 78l { Search_map.rel_pos = 46 };
      let hashes =
        {
          Search_map.ofs = 1234;
          hashes = Hashes.HashSet.of_list [ 34l; 78l ];
          positions;
        }
      in
      assert_equal
        (Some
           {
             Search.match_start = 1234;
             match_stop = 1234;
             match_id = 1;
             match_offset = 12;
           })
        (Search.best_match ~debug:false search_map hashes);
      assert_equal true !search_called;
      let search _ = [ []; [] ] in
      let search_map = Search_map.init search in
      assert_equal None (Search.best_match ~debug:false search_map hashes) );
    ( "Buffered match" >:: fun _ ->
      let params =
        { Search.default_params with Search.buffer_size = 4; threshold = 2 }
      in
      let m start id ofs =
        Some
          {
            Search.match_start = start;
            match_stop = start;
            match_id = id;
            match_offset = ofs;
          }
      in
      let content = Ringbuffer.init [| m 1 3 4; m 2 3 5; m 3 1 2; m 4 5 6 |] in
      assert_equal
        (Some
           {
             Search.match_start = 1;
             match_stop = 2;
             match_id = 3;
             match_offset = 4;
           })
        (Search.buffered_match ~params content);
      let content = Ringbuffer.init [| m 1 3 4; None; m 2 1 2; m 3 5 6 |] in
      assert_equal None (Search.buffered_match ~params content) );
    ( "Frames" >:: fun _ ->
      let mk pos hash = { Hashes.pos; hash } in
      let hashes =
        IStream.make
          [ mk 1 2l; mk 2 4l; mk 3 6l; mk 7 8l; mk 9 10l; mk 10 12l; mk 13 14l ]
      in
      let audio_params =
        { Audio.default_params with Audio.frame_step = 0.01 }
      in
      let params =
        {
          Search.default_params with
          Search.frame_length = 2.0 *. audio_params.Audio.frame_step;
          frame_step = audio_params.Audio.frame_step;
        }
      in
      let frames = Search.frames ~params ~audio_params hashes in
      let frame_equal ofs hash_list = function
        | None -> assert_failure "No frame!"
        | Some frame ->
            assert_equal ofs frame.Search_map.ofs;
            assert_equal (List.map snd hash_list)
              (Hashes.HashSet.elements frame.Search_map.hashes);
            let h =
              Hashtbl.fold
                (fun hash { Search_map.rel_pos; _ } ret ->
                  (rel_pos, hash) :: ret)
                frame.Search_map.positions []
            in
            assert_equal hash_list (List.rev h)
      in
      frame_equal 1 [ (0, 2l); (1, 4l) ] (frames ());
      frame_equal 2 [ (0, 4l); (1, 6l) ] (frames ());
      frame_equal 3 [ (0, 6l) ] (frames ());
      frame_equal max_int [] (frames ());
      frame_equal max_int [] (frames ());
      frame_equal 7 [ (0, 8l) ] (frames ());
      frame_equal 7 [ (0, 8l) ] (frames ());
      frame_equal 9 [ (0, 10l) ] (frames ());
      frame_equal 9 [ (0, 10l); (1, 12l) ] (frames ());
      frame_equal 10 [ (0, 12l) ] (frames ());
      frame_equal max_int [] (frames ());
      frame_equal 13 [ (0, 14l) ] (frames ());
      frame_equal 13 [ (0, 14l) ] (frames ());
      assert_equal None (frames ());
      assert_equal None (frames ()) );
  ]
