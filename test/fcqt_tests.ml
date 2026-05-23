open OUnit2

let suite =
  let fcqt_exec ~reassign descr =
    descr >:: fun _ ->
    let params =
      {
        Fcqt.min_freq = 110.0;
        max_freq = 1760.0;
        bins_per_octave = 12.0;
        samplerate = 11025.0;
        step = 0.025;
        reassign;
      }
    in
    let fcqt = Fcqt.init params in
    assert_raises (Failure "Invalid input size!") (fun () ->
        Fcqt.execute fcqt [||]);
    let peak freq =
      let fcqt = Fcqt.init params in
      let pi = 4.0 *. atan 1.0 in
      let sin n =
        sin (2.0 *. pi *. float n *. freq /. params.Fcqt.samplerate)
      in
      let rec f () =
        try
          let data = Array.init (Fcqt.sample_size fcqt) sin in
          let output = Fcqt.execute fcqt data in
          let _, (max_pos, _) =
            Array.fold_left
              (fun (cur_pos, (pos, max)) v ->
                let max = if max < v then (cur_pos, v) else (pos, max) in
                (cur_pos + 1, max))
              (0, (-1, 0.0))
              output
          in
          max_pos
        with Fcqt.Need_more_data -> f ()
      in
      f ()
    in
    assert_equal 29 (peak 587.33);
    assert_equal 3 (peak 130.81);
    assert_equal 24 (peak 440.0);
    assert_equal 43 (peak 1318.5)
  in
  [
    ( "FCQT initialization" >:: fun _ ->
      let params =
        {
          Fcqt.min_freq = 640.0;
          max_freq = 1700.0;
          bins_per_octave = 24.0;
          samplerate = 11025.0;
          step = 0.025;
          reassign = true;
        }
      in
      let fcqt = Fcqt.init params in
      assert_equal 1024 (Fcqt.sample_size fcqt) );
    fcqt_exec ~reassign:false "FCQT execution without reassigning";
    fcqt_exec ~reassign:true "FCQT execution with reassigning";
  ]
