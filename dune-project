(lang dune 2.5)
(generate_opam_files true)

(name ocaml-r)
(source (github pveber/ocaml-r))
(homepage "https://github.com/pveber/ocaml-r/")
(bug_reports "https://github.com/pveber/ocaml-r/issues")
(license GPL3)
(authors
  "Guillaume Yzyquel"
  "Maxence Guesdon"
  "Philippe Veber")
(maintainers "philippe.veber@gmail.com")
(documentation "https://pveber.github.io/ocaml-r/ocaml-r/index.html")

(package
  (name ocaml-r)
  (synopsis "Objective Caml bindings for the R interpreter")
  (description "
OCaml-R is a library that can be used to construct R values in memory,
convert them to OCaml values, and build clean wrappers to R
functions. It provide a simple means to develop bindings to any R
package.")
  (tags (R statistics))
  (depends
    (alcotest :with-test)
    (base (and :build (>= v0.14)))
    (conf-r (and :build))
    (conf-r-mathlib (and :build))
    (dune (>= 2.5))
    (ocaml (>= 4.08))
    (stdio (and (>= v0.14) :build))))
