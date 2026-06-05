let () =
  if Sys.argv.(1) = "true" then
    Printf.printf
      "(executable\n\
      \ (name pitch_shift)\n\
      \ (modules pitch_shift)\n\
      \ (libraries ithaca_utils soundtouch))\n"
