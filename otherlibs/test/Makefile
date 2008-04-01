#########################################################################
#                                                                       #
#                            Objective Caml                             #
#                                                                       #
#            Xavier Leroy, projet Cristal, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 1999 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the GNU Library General Public License, with     #
#   the special exception on linking described in file ../../LICENSE.   #
#                                                                       #
#########################################################################

# $Id$

# Makefile for the test library

include ../../config/Makefile

# Compilation options
CC=$(BYTECC)
CFLAGS=-O -I../../byterun $(BYTECCCOMPOPTS) $(SHAREDCCCOMPOPTS)
CAMLC=../../ocamlcomp.sh
CAMLOPT=../../ocamlcompopt.sh
COMPFLAGS=-warn-error A -g
COBJS=
MKLIB=../../boot/ocamlrun ../../tools/ocamlmklib

all: libtest.a test.cmi test.cma

allopt: libtest.a test.cmi test.cmxa

libtest.a: $(COBJS)
	$(MKLIB) -o test $(COBJS)

test.cma: test.cmo
	$(MKLIB) -ocamlc '$(CAMLC)' -o test test.cmo

test.cmxa: test.cmx
	$(MKLIB) -ocamlopt '$(CAMLOPT)' -o test test.cmx

test.cmx: ../../ocamlopt

partialclean:
	rm -f *.cm*

clean: partialclean
	rm -f *.a *.so *.o

install:
	if test -f dlltest.so; then cp dlltest.so $(STUBLIBDIR)/dlltest.so; fi
#	cp libtest.a $(LIBDIR)/libtest.a
#	cd $(LIBDIR); $(RANLIB) libtest.a
	cp test.cma test.cmi test.mli $(LIBDIR)

installopt:
	cp test.cmx test.cmxa $(LIBDIR)
	cd $(LIBDIR); $(RANLIB) test.a

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.mli.cmi:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLC) -c $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

depend:
#	gcc -MM $(CFLAGS) *.c > .depend
	../../boot/ocamlrun ../../tools/ocamldep *.mli *.ml >> .depend

include .depend
