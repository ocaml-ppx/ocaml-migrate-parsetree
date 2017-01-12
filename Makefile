# This file is part of the migrate-parsetree package. It is released under the
# terms of the MIT license (see LICENSE file).
# Copyright 2017  Frédéric Bour
#
# It is forked from ppx-tools package, which is copyright 2013
# Alain Frisch and LexiFi.

include $(shell ocamlc -where)/Makefile.config

# Don't forget to change META file as well
PACKAGE = ocaml-migrate-parsetree
VERSION = 0.1

# Config
OCAMLC = ocamlc -bin-annot
OCAMLOPT = ocamlopt
COMPFLAGS = -w +A-4-17-44-45-105-42 -I src -I +compiler-libs -safe-string

# Files
OCAML_ASTS= \
  src/ast_404.ml \
	src/ast_403.ml \
	src/ast_402.ml

OBJECTS= \
	src/migrate_parsetree_def.cmo \
  $(OCAML_ASTS:.ml=.cmo) \
	src/migrate_parsetree_403_404.cmo \
	src/migrate_parsetree_404_403.cmo \
	src/migrate_parsetree_402_403.cmo \
	src/migrate_parsetree_403_402.cmo \
	src/migrate_parsetree.cmo

OCAML_VERSION=$(shell ./ast_version.sh $(OCAMLC))

.PHONY: all
all: migrate_parsetree.cma migrate_parsetree.cmxa

.PHONY: clean
clean:
	rm -f src/*.cm* src/*.o src/*.obj src/*.a src/*.lib
	rm -f migrate_parsetree.* src/migrate_parsetree.ml src/migrate_parsetree.mli
	rm -f $(OCAML_ASTS) src/ast_current.ml

# Default rules

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(COMPFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(COMPFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(COMPFLAGS) -c $<

# Install/uninstall

targets = $(1).mli $(1).cmi $(1).cmt $(1).cmti $(wildcard $(1).cmx)
INSTALL = META \
   migrate_parsetree.cma \
   $(wildcard migrate_parsetree.cmxa migrate_parsetree$(EXT_LIB)) \
	 $(OBJECTS:.cmo=.cmi) $(wildcard $(OBJECTS:.cmo=.cmt) $(OBJECTS:.cmo=.cmti))

.PHONY: reinstall install uninstall

install:
	ocamlfind install $(PACKAGE) $(INSTALL)

uninstall:
	ocamlfind remove $(PACKAGE)

reinstall:
	$(MAKE) uninstall
	$(MAKE) install

# Ast selection

src/ast_$(OCAML_VERSION).ml: asts/ast_current.ml
	cp $< $@
	echo 'let version : Migrate_parsetree_def.ocaml_version = `OCaml_$(OCAML_VERSION)' >> $@

src/ast_%.ml: asts/ast_%.ml
	cp $< $@

src/migrate_parsetree.mli: src/migrate_parsetree.mli.in
src/migrate_parsetree.ml: src/migrate_parsetree.ml.in

src/migrate_parsetree.ml src/migrate_parsetree.mli:
	cp $< $@
	echo 'module Ast_current = Ast_$(OCAML_VERSION)' >> $@

$(OCAML_ASTS:.ml=.cmo): $(OCAML_ASTS)

migrate_parsetree.cma: $(OBJECTS)
	$(OCAMLC) -a -o migrate_parsetree.cma $^

migrate_parsetree.cmxa: $(OBJECTS:.cmo=.cmx)
	$(OCAMLOPT) -a -o migrate_parsetree.cmxa $^

.PHONY: depend
depend: $(OCAML_ASTS)
	ocamldep -I src/ src/*.ml src/*.mli > .depend
	dos2unix .depend
-include .depend

## gencopy from ppx_tools package
## ./gencopy -I . -map Ast_403:Ast_404 Ast_403.Parsetree.expression > migrate_parsetree_403_404.ml
## ./gencopy -I . -map Ast_404:Ast_403 Ast_404.Parsetree.expression > migrate_parsetree_404_403.ml
## ./gencopy -I . -map Ast_402:Ast_403 Ast_402.Parsetree.expression > migrate_parsetree_402_403.ml
## ./gencopy -I . -map Ast_403:Ast_402 Ast_403.Parsetree.expression > migrate_parsetree_403_402.ml

