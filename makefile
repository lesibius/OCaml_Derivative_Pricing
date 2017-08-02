OCB = ocamlbuild
OCB_FLAGS = -use-ocamlfind -tags thread

doc.odocl/index.html: doc.odocl
	$(OCB) $(OCB_FLAGS) $@

clean:
	$(OCB) -clean

.PHONY: clean
