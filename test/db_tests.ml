open OUnit2

let make_db tmpfile =
  let params =
    { Db.max_id_per_hash = 1024; max_pos_per_hash = 50; saturate = true }
  in
  Db.make params (Lmdb_store.operations tmpfile)

let get_db ctx =
  let tmpfile = Filename.temp_file "ithaca-test" ".db" in
  let set_up _ = make_db tmpfile in
  let tear_down _ _ = try Sys.remove tmpfile with _ -> () in
  bracket set_up tear_down ctx

let suite =
  [
    ( "Hashes DB insert/search" >:: fun ctx ->
      let db = get_db ctx in
      let hashes =
        IStream.make
          [
            { Hashes.pos = 2; hash = 228002194 };
            { Hashes.pos = 4; hash = -1366283977 };
            { Hashes.pos = 0; hash = -840465741 };
            { Hashes.pos = 0; hash = -892327182 };
            { Hashes.pos = 1; hash = -291067170 };
          ]
      in
      db.Db.insert [ (1234, hashes) ];
      let hashes =
        [ 228002194; -1366283977; -840465741; -892327182; -291067170 ]
      in
      ignore (db.Db.search hashes) );
  ]
