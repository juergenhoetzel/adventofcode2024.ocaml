(lang dune 3.16)

(name aoc)

(generate_opam_files true)

(source
 (github juergenhoetzel/adventofcode2024.ocaml))

(authors "Jürgen Hötzel")

(maintainers "Jürgen Hötzel")

(package
 (name aoc)
 (synopsis "Advend of Code")
 (description "Solutions in OCaml")
 (depends ocaml dune base core))

