(lang dune 3.17)

(name toyc_compiler)

(using menhir 2.0)

(generate_opam_files true)

(package
 (name toyc_compiler)
 (depends
  (ocaml
   (>= 4.14.0))
  (dune
   (>= 3.0))
  menhir
  menhirlib))
