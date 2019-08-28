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

.PHONY: test
test:
	ocamlbuild -pkgs oUnit -I $(SRCDIR) $(TESTDIR)/eval_test.$(BUILD)
	./eval_test.native

clean:
	$(OCAMLBUILD) -clean
