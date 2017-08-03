OCB = ocamlbuild
OCB_FLAGS = -use-ocamlfind -tags thread

test.native: $(wildcard *.ml) $(wildcard *.mli)
	$(OCB) $(OCB_FLAGS) $@

doc.odocl/index.html: doc.odocl
	$(OCB) $(OCB_FLAGS) $@

clean:
	$(OCB) -clean

.PHONY: clean
