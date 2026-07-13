(lang dune 3.22)
(name ithaca)

(generate_opam_files true)
(opam_file_location inside_opam_directory)

(package
 (name ithaca)
 (synopsis "Audio fingerprinting library and tools")
 (license "GPL-3.0-or-later")
 (description
  "OCaml implementation of the scalable audio fingerprinting algorithm (Fenet et al., ISMIR 2011).")
 (depends
  (ocaml (>= "5.1"))
  (dune (>= "3.22"))
  (atdgen :build)
  atdgen-runtime
  cryptokit
  fftw3
  samplerate
  (ounit2 :with-test)
  (soundtouch :with-test)
  (imagelib :with-doc))
 )
