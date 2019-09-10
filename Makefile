OCAMLBUILD=ocamlbuild

# Should we build native or bytecode by default?
BUILD=native
# BUILD=byte

SRCDIR = src
TESTDIR = test

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
test: test_systemf

.PHONY: test_systemf
test_systemf:
	$(OCAMLBUILD) -pkgs oUnit -I $(SRCDIR) -I $(SRCDIR)/systemf $(TESTDIR)/systemf/eval_test.$(BUILD) $(TESTDIR)/systemf/type_check_test.$(BUILD)
	./eval_test.native
	./type_check_test.native

example: all example_systemf

.PHONY: example_systemf
example_systemf:
	./systemf.native $(SRCDIR)/systemf/example.systemf

clean:
	$(OCAMLBUILD) -clean
