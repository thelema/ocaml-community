
SOURCES = tAPDocument.mli tAPDocument.ml \
		  testBuilder.mli testBuilder.ml \
		  testSimple.mli testSimple.ml
RESULT  = testSimple

PACKS = extlib

LIBINSTALL_FILES = tAPDocument.mli tAPDocument.cmi \
            	   testBuilder.mli testBuilder.cmi \
		           testSimple.mli testSimple.cmi \
		           testSimple.cma testSimple.cmxa testSimple.a

INCDIRS = /usr/local/lib/ocaml/site-lib/extlib
OCAMLBLDFLAGS = extLib.cma

all: bcl

opt: ncl

test: all
	prove t/*.ml

-include OCamlMakefile
