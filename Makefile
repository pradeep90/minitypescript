OCAMLBUILD=ocamlbuild

# Should we build native or bytecode by default?
BUILD=native
# BUILD=byte

SRCDIR = src
TESTDIR = test
BINDIR = bin

default:
	@echo "To compile all languages run:                 make all"
	@echo "To compile a single language <lang> run:      make <lang>"
	@echo "To compile bytecode add BUILD=byte:           make BUILD=byte ..."
	@echo "Available languages:"
	@echo "$(sort $(LANGS))"

LANGS = $(shell find src -mindepth 1 -maxdepth 1 -type d -exec basename {} \;)

all: $(LANGS)

$(LANGS): % :
	$(OCAMLBUILD) -use-menhir -menhir "menhir --explain" -libs unix -I $(SRCDIR) src/$@/$@.$(BUILD)

.PHONY: repl
repl:
	ocaml -I _build/src

.PHONY: test
test: test_systemf test_fomega test_uniontypes

.PHONY: test_systemf
test_systemf:
	$(OCAMLBUILD) -pkgs oUnit -I $(SRCDIR) -I $(SRCDIR)/systemf $(TESTDIR)/systemf/eval_test.$(BUILD) $(TESTDIR)/systemf/type_check_test.$(BUILD)
	mv ./eval_test.native ./type_check_test.native $(BINDIR)/systemf
	$(BINDIR)/systemf/eval_test.native
	$(BINDIR)/systemf/type_check_test.native

.PHONY: test_uniontypes
test_uniontypes:
	$(OCAMLBUILD) -pkgs oUnit -I $(SRCDIR) -I $(SRCDIR)/uniontypes $(TESTDIR)/uniontypes/eval_test.$(BUILD) $(TESTDIR)/uniontypes/type_check_test.$(BUILD)
	mv ./eval_test.native ./type_check_test.native $(BINDIR)/uniontypes
	$(BINDIR)/uniontypes/eval_test.native
	$(BINDIR)/uniontypes/type_check_test.native

.PHONY: test_fomega
test_fomega:
	$(OCAMLBUILD) -pkgs oUnit -I $(SRCDIR) -I $(SRCDIR)/fomega -install-bin-dir bin/fomega $(TESTDIR)/fomega/eval_test.$(BUILD) $(TESTDIR)/fomega/type_check_test.$(BUILD)
	mv ./eval_test.native ./type_check_test.native $(BINDIR)/fomega
	$(BINDIR)/fomega/eval_test.native
	$(BINDIR)/fomega/type_check_test.native

example: all example_systemf example_fomega example_uniontypes

.PHONY: example_systemf
example_systemf:
	./systemf.native $(SRCDIR)/systemf/example.systemf

.PHONY: example_uniontypes
example_uniontypes:
	./uniontypes.native $(SRCDIR)/uniontypes/example.uniontypes

.PHONY: black_box_tests_uniontypes
black_box_tests_uniontypes:
	./uniontypes.native $(SRCDIR)/uniontypes/black_box_tests.uniontypes

.PHONY: example_fomega
example_fomega:
	./fomega.native $(SRCDIR)/fomega/example.fomega

clean:
	$(OCAMLBUILD) -clean
	rm -f $(BINDIR)/systemf/* $(BINDIR)/fomega/*
