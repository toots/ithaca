open OUnit2

let () =
  run_test_tt_main
    ("Ithaca tests"
    >::: List.concat
           [
             Float_buffer_tests.suite;
             Ringbuffer_tests.suite;
             Fcqt_tests.suite;
             Hashes_tests.suite;
             Quads_tests.suite;
             Db_tests.suite;
             Search_tests.suite;
           ])
