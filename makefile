OCB = ocamlbuild
OCB_PATHS = -I src -I test
OCB_FLAGS = -use-ocamlfind -tags thread $(OCB_PATHS)

test.byte: $(wildcard *.ml) $(wildcard *.mli)
	$(OCB) closedform.o $(OCB_PATHS)
	$(OCB) $(OCB_FLAGS) $@ 

core.docdir/index.html: 
	$(OCB) $(OCB_FLAGS) $@

clean:
	$(OCB) -clean

.PHONY: clean
