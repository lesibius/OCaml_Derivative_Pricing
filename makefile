OCB = ocamlbuild
OCB_FLAGS = -use-ocamlfind -tags thread

test.native: $(wildcard *.ml) $(wildcard *.mli)
	$(OCB) $(OCB_FLAGS) $@

core.docdir/index.html: 
	$(OCB) $(OCB_FLAGS) $@

clean:
	$(OCB) -clean

.PHONY: clean
