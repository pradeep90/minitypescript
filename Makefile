OCAMLBUILD=ocamlbuild

# Should we build native or bytecode by default?
BUILD=native
# BUILD=byte

SRCDIR = src
TESTDIR = test

default:
	@echo "To compile MiniTypeScript, run:                 make all"

all: minitypescript

.PHONY: minitypescript
minitypescript:
	$(OCAMLBUILD) -use-menhir -menhir "menhir --explain" -libs unix -I $(SRCDIR) $(SRCDIR)/minitypescript.$(BUILD)

.PHONY: repl
repl:
	ocaml -I _build/src

.PHONY: test
test:
	ocamlbuild -pkgs oUnit -I $(SRCDIR) $(TESTDIR)/eval_test.$(BUILD) $(TESTDIR)/type_check_test.$(BUILD)
	./eval_test.native
	./type_check_test.native

example: all
	./minitypescript.native $(SRCDIR)/example.minits

clean:
	$(OCAMLBUILD) -clean
