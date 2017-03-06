# This file is part of the migrate-parsetree package. It is released under the
# terms of the LGPL 2.1 license (see LICENSE file).
# Copyright 2017  Frédéric Bour
#
# It is forked from ppx-tools package, which is copyright 2013
# Alain Frisch and LexiFi.

# Don't forget to change META file as well
PACKAGE = ocaml-migrate-parsetree
VERSION = 0.3

# Config
include $(shell ocamlc -where)/Makefile.config
OCAML_VERSION=$(shell ./ast_version.sh ocamlc)

ifeq ($(OCAML_VERSION),402)
OCAMLC = ocamlfind c -bin-annot
OCAMLOPT = ocamlfind opt
COMPFLAGS = -w +A-4-17-44-45-105-42 -I src -safe-string -package compiler-libs -package result -no-alias-deps -open Result
else
OCAMLC = ocamlc -bin-annot
OCAMLOPT = ocamlopt
COMPFLAGS = -w +A-4-17-44-45-105-42 -I src -I +compiler-libs -no-alias-deps -safe-string
endif

# Files
MIGRATE_OBJECTS= \
	src/migrate_parsetree_def.cmo \
	src/ast_402.cmo \
	src/ast_403.cmo \
	src/ast_404.cmo \
	src/ast_405.cmo \
	src/migrate_parsetree_402_403_migrate.cmo \
	src/migrate_parsetree_403_402_migrate.cmo \
	src/migrate_parsetree_403_404_migrate.cmo \
	src/migrate_parsetree_404_403_migrate.cmo \
	src/migrate_parsetree_404_405_migrate.cmo \
	src/migrate_parsetree_405_404_migrate.cmo \
	src/migrate_parsetree_402_403.cmo \
	src/migrate_parsetree_403_402.cmo \
	src/migrate_parsetree_403_404.cmo \
	src/migrate_parsetree_404_403.cmo \
	src/migrate_parsetree_404_405.cmo \
	src/migrate_parsetree_405_404.cmo \
	src/migrate_parsetree_versions.cmo \
	src/migrate_parsetree_ast_io.cmo \
	src/migrate_parsetree.cmo

DRIVER_OBJECTS= \
	src/migrate_driver.cmo src/migrate_driver_main.cmx

OBJECTS=$(MIGRATE_OBJECTS) $(DRIVER_OBJECTS)

.PHONY: all
all: migrate_parsetree.cma migrate_parsetree.cmxa $(DRIVER_OBJECTS)

ifeq ($(NATDYNLINK),true)
all: migrate_parsetree.cmxs
endif

.PHONY: clean
clean:
	rm -f src/*.cm* src/*.o src/*.obj src/*.a src/*.lib
	rm -f tools/*.cm* tools/*.o tools/*.obj tools/*.a tools/*.lib tools/*.native
	rm -f migrate_parsetree.*

.PHONY: cinaps
cinaps:
	cinaps -styler ocp-indent -i src/migrate_parsetree_versions.ml*
	cinaps -styler ocp-indent -i src/migrate_parsetree_40?_40?.ml*

# Default rules

.SUFFIXES: .ml .mli .cmo .cmi .cmx .native

PP = -pp "sh pp.sh $(OCAML_VERSION)"

%.cmo: %.ml
	$(OCAMLC) $(COMPFLAGS) $(PP) -c $<

%.cmi: %.mli
	$(OCAMLC) $(COMPFLAGS) $(PP) -c $<

%.cmx: %.ml
	$(OCAMLOPT) $(COMPFLAGS) $(PP) -c $<

.cmx.native: src/migrate_parsetree.cmxa
	$(OCAMLOPT) $(COMPFLAGS) ocamlcommon.cmxa migrate_parsetree.cmxa $< -o $@

# Install/uninstall

targets = $(1).mli $(1).cmi $(1).cmt $(1).cmti $(wildcard $(1).cmx)
INSTALL = META \
	$(wildcard migrate_parsetree.*) \
	$(OBJECTS:.cmo=.cmi) $(wildcard $(OBJECTS:.cmo=.cmx)) \
	$(wildcard $(OBJECTS:.cmo=.cmt) $(OBJECTS:.cmo=.cmti))

.PHONY: reinstall install uninstall

install:
	ocamlfind install $(PACKAGE) $(INSTALL)

uninstall:
	ocamlfind remove $(PACKAGE)

reinstall:
	$(MAKE) uninstall
	$(MAKE) install

# Ast selection

migrate_parsetree.cma: $(MIGRATE_OBJECTS)
	$(OCAMLC) -a -o $@ $^

migrate_parsetree.cmxa: $(MIGRATE_OBJECTS:.cmo=.cmx)
	$(OCAMLOPT) -a -o $@ $^

migrate_parsetree.cmxs: $(MIGRATE_OBJECTS:.cmo=.cmx)
	$(OCAMLOPT) -shared -o $@ $^

# Auxiliary tools
tools: tools/add_special_comments.native

tools/add_special_comments.native: tools/add_special_comments.ml
	ocamlfind opt -o $@ -linkpkg -package compiler-libs.common $<

## gencopy from ppx_tools package
## ./gencopy -I . -map Ast_403:Ast_404 Ast_403.Parsetree.expression > migrate_parsetree_403_404.ml
## ./gencopy -I . -map Ast_404:Ast_403 Ast_404.Parsetree.expression > migrate_parsetree_404_403.ml
## ./gencopy -I . -map Ast_402:Ast_403 Ast_402.Parsetree.expression > migrate_parsetree_402_403.ml
## ./gencopy -I . -map Ast_403:Ast_402 Ast_403.Parsetree.expression > migrate_parsetree_403_402.ml

.PHONY: depend
depend:
	ocamldep -I src/ src/*.ml src/*.mli > .depend
	dos2unix .depend
-include .depend
