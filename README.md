# OCaml-R: Objective Caml bindings for the R interpreter

## Description

OCaml-R is an Objective Caml binding embedding R's interpreter into
OCaml code. It also provides bindings to R's standalone mathematical
library, and has embryonic support for R's standard libraries.

It can be used to build R datastructures in memory, call R functions
and convert the returned value to OCaml datastructures.

## Installation

```sh
opam install ocaml-r
```

## Library usage

Under `utop`:

```ocaml
# #require "ocaml-r.interpreter";;
# open OCamlR;;
```

**WARNING** `#require "ocaml-r"` is not sufficient to have the R
interpreter properly initialised. Using functions from the library
after this will typically lead to a segmentation fault. The
`R.interpreter` package depends on oCamlR.cmo or oCamlR.cmx, and when
loaded, this module properly initialises the R interpreter. So you
only have to #require "R.interpreter" to have R up and running.

To access functions from package `foo`, load `ocaml-r.foo`, e.g.:
```ocaml
# #require "ocaml-r.stats";;
# OCamlR_stats.rnorm 2;;
- : float array = [|-0.533469267471010089; 0.554898389717302543|]
```

There are other packages. `ocaml-r.math` links to the standalone
mathematical shared library of R. Other packages, such as R.base and
R.stats are embryonic bindings of R's standard library. Help is very
welcome on this side.



Authorship
----------

OCaml-R was initially written by Maxence Guesdon. It was a rather
simple binding that was used essentially by feeding strings to the R
interpreter parser and evaluation function, and providing data
conversion functions for simple R types. This was version 0.1 of
OCaml-R.

The current version 0.2, is essentially an almost complete rewrite
by Guillaume Yziquel providing tight integration with the R library.
It can dissect R values to expose their internal structures to OCaml
(though this shouldn't be the most useful aspect, nor the recommended
way to use this binding), it construct R calls by building up R values
directly, integrates OCaml's and R's garbage collectors (though somewhat
poorly), chains R exceptions back to Objective Caml, and provides static
initialisation of the R interpreter.

Starting 2012, Philippe Veber took over the maintainance.


Copyright 2008-2010 INRIA - Maxence Guesdon.
Contact: maxence.guesdon@inria.fr

Copyright 2009-2010 Guillaume Yziquel.
Contact: guillaume.yziquel@citycable.ch

Copyright 2011-2018 Philippe Veber.
Contact: philippe.veber@gmail.com

Licenced under the GPL version 3.
