OCB = ocamlbuild
OCB_FLAGS = -use-ocamlfind -tags thread

clean:
	$(OCB) -clean

.PHONY: clean
