let () =
  if Sys.argv.(1) = "true" then
    Printf.printf
      "(executable\n\
      \ (name visualize)\n\
      \ (modules visualize)\n\
      \ (libraries ithaca_args ithaca_audio ithaca_utils ithaca_lmdb \
       imagelib.unix))\n"
