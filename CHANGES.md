OCaml-R release notes
=====================

ocaml-r-0.4.0 2020-10-10
------------------------

This version implements a massive API refactoring, which generalizes
and amplifies the design experimented in version 0.2.0. Each R
datatype is represented as an abstract type of a dedicated module. The
`t` type of the module is a bare SEXP (or at rather a custom block
wrapping a bare SEXP). S3 inheritance is represented by module (type)
inclusion.

ocaml-r-0.3.1 2020-07-29
------------------------

- replaced configurator by dune.configurator
- fixed compilation warnings

ocaml-r-0.3.0 2020-05-07
------------------------

- base: added readRDS, saveRDS, cbind, rbind
- graphics: abline
- stats: ecdf
- fixed compilation bug

ocaml-r-0.2.0 2019-06-07
------------------------

- experimented modular (instead of object) interfaces
- a few more wrappers in base, stats and graphics
- improved stub generation
- API documentation

ocaml-r-0.1.1 2018-11-18
------------------------

jbuilder-to-dune transition

ocaml-r-0.1.0 2018-06-06
------------------------

First formal release


Former Changelog
================

2010-02-09 Maxence Guesdon  <Maxence.Guesdon@inria.fr>

	* update website, clean configure and build process
	* generate documentation

2010-02-08  Guillaume Yziquel  <guillaume.yziquel@citycable.ch>

	* release 0.2
	* major rewrite of OCaml-R

2009-09-15  Maxence Guesdon  <Maxence.Guesdon@inria.fr>

	* release 0.1

2009-09-14  Maxence Guesdon  <Maxence.Guesdon@inria.fr>

	* add: ChangeLog
	* mod: use ocaml{c,opt} instead of ocamlmklib
	* new: projet ocaml-r hosted on gna.org

