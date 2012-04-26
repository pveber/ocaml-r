#################################################################################
#                OCaml-R                                                        #
#                                                                               #
#    Copyright (C) 2008-2010 Institut National de Recherche en                  #
#    Informatique et en Automatique. All rights reserved.                       #
#                                                                               #
#    Copyright (C) 2009-2010 Guillaume Yziquel. All rights reserved.            #
#                                                                               #
#    This program is free software; you can redistribute it and/or modify       #
#    it under the terms of the GNU General Public License as                    #
#    published by the Free Software Foundation; either version 3 of the         #
#    License, or  any later version.                                            #
#                                                                               #
#    This program is distributed in the hope that it will be useful,            #
#    but WITHOUT ANY WARRANTY; without even the implied warranty of             #
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               #
#    GNU Library General Public License for more details.                       #
#                                                                               #
#    You should have received a copy of the GNU General Public                  #
#    License along with this program; if not, write to the Free Software        #
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   #
#    02111-1307  USA                                                            #
#                                                                               #
#    Contact: Maxence.Guesdon@inria.fr                                          #
#             guillaume.yziquel@citycable.ch                                    #
#################################################################################

RLIBDIR = /usr/lib/R/lib
RINCLUDE = /usr/share/R/include

all: byte native

byte: r.cmi r.cmo r.cma oCamlR.cmo
	make -C math byte
	make -C r-base byte
	make -C r-stats byte
	make -C r-methods byte
	make -C r-graphics byte
	make -C r-grDevices byte

native: r.cmi r.cmx r.cmxa oCamlR.cmx
	make -C math native
	make -C r-base native
	make -C r-stats native
	make -C r-methods native
	make -C r-graphics native
	make -C r-grDevices native

clean:
	rm -f standard.ml *.o *.a *.so *.cmi *.cmo *.cmx *.cma *.cmxa
	make -C math clean
	make -C r-base clean
	make -C r-stats clean
	make -C r-methods clean
	make -C r-graphics clean
	make -C r-grDevices clean

standard.ml: standard.R
	R --silent --vanilla --slave < $< > $@

r.cmi: r.mli
	ocamlfind ocamlc r.mli

r.cmo: r.ml r.cmi standard.ml
	ocamlfind ocamlc -c -package camlp4.macro -syntax camlp4o r.ml

r.cmx: r.ml r.cmi standard.ml
	ocamlfind ocamlopt -c -package camlp4.macro -syntax camlp4o r.ml

r.cma: dllr_stubs.so r.cmo
	ocamlfind ocamlc -verbose -a -dllpath $(RLIBDIR) -dllib dllr_stubs.so -dllib libR.so -o r.cma r.cmo

r.cmxa: dllr_stubs.so r.cmx
	ocamlfind ocamlopt -verbose -a -ccopt -L$(RLIBDIR) -cclib -lr_stubs -cclib -lR -o r.cmxa r.cmx

libr_stubs.a: r_stubs.o
	ar rcs libr_stubs.a r_stubs.o

dllr_stubs.so: libr_stubs.a r_stubs.o
	ocamlmklib -verbose -o r_stubs r_stubs.o

r_stubs.o: r_stubs.c
	ocamlopt -verbose -ccopt -Werror -ccopt -fPIC -ccopt -I$(RINCLUDE) -c r_stubs.c

oCamlR.cmo: oCamlR.ml r.cmi
	ocamlfind ocamlc -c oCamlR.ml
	rm -f oCamlR.cmi

oCamlR.cmx: oCamlR.ml r.cmi
	ocamlfind ocamlopt -c oCamlR.ml
	rm -f oCamlR.cmi

librmath_stubs.a: math/rmath_stubs.o
	ar rcs librmath_stubs.a math/rmath_stubs.o

dllrmath_stubs.so: librmath_stubs.a math/rmath_stubs.o
	ocamlmklib -verbose -o rmath_stubs math/rmath_stubs.o

rmath.cma: dllrmath_stubs.so math/rmath.cmo
	$(OCAMLC) -verbose -a -dllib dllrmath_stubs.so -dllib libRmath.so -o rmath.cma math/rmath.cmo

rmath.cmxa: dllrmath_stubs.so math/rmath.cmx
	$(OCAMLOPT) -verbose -a -cclib -lrmath_stubs -cclib -lRmath -o rmath.cmxa math/rmath.cmx

math/rmath_stubs.o: math/rmath_stubs.c

RBASE_ML_SOURCES= \
	  r-base/main.ml        \
	  r-base/listing.ml     \
	  r-base/dataFrame.ml   \
	  r-base/date.ml
rbase.ml: rbase.mli $(RBASE_ML_SOURCSE)
	cat $(RBASE_ML_SOURCES) > $@

rbase.mli: \
	  r-base/main.mli       \
	  r-base/listing.mli    \
	  r-base/dataFrame.mli  \
	  r-base/date.mli
	cat $^ > $@

rstats.ml: rstats.mli r-stats/main.ml
	cat r-stats/main.ml > $@

rstats.mli: r-stats/main.mli
	cat $^ > $@

rmethods.ml: rmethods.mli r-methods/main.ml
	cat r-methods/main.ml > $@

rmethods.mli: r-methods/main.mli
	cat $^ > $@

test:
	ocaml -init ocamlinit

debug:
	gdb --args ocamlrun ocaml -init ocamlinit

doc: r.mli rbase.mli rstats.mli math/rmath.mli
	$(MKDIR) ocamldoc
	ocamlfind ocamldoc -package calendar $(INCLUDES) \
	-html -d ocamldoc $^

install: all remove
	ocamlfind install R META *.[oa] *.so *.cm[axi] *.cmxa oCamlR.cmo \
	  math/*.[oa] math/*.so math/*.cm[axi] math/*.cmxa \
	  r-base/_build/*.[oa] r-base/_build/*.cm[axi] r-base/_build/*.cmxa \
	  r-stats/_build/*.[oa] r-stats/_build/*.cm[axi] r-stats/_build/*.cmxa \
	  r-graphics/_build/*.[oa] r-graphics/_build/*.cm[axi] r-graphics/_build/*.cmxa \
	  r-methods/_build/*.[oa] r-methods/_build/*.cm[axi] r-methods/_build/*.cmxa \
	  r-grDevices/_build/*.[oa] r-grDevices/_build/*.cm[axi] r-grDevices/_build/*.cmxa

remove:
	ocamlfind remove R

