OCB = ocamlbuild
OCB_FLAGS = -use-ocamlfind -tags thread -I src -I test

test.native: $(wildcard *.ml) $(wildcard *.mli)
	$(OCB) $(OCB_FLAGS) $@

c_test: $(wildcard src/*.c) $(wildcard src/*.h) test/c_test.c
	gcc src/closedform.c src/closedform.h test/c_test.c -lm -o $@

core.docdir/index.html: 
	$(OCB) $(OCB_FLAGS) $@

clean:
	$(OCB) -clean

.PHONY: clean
