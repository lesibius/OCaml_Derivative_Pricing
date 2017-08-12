OCB = ocamlbuild
OCB_FLAGS = -use-ocamlfind -tags thread

test.native: math_util.cmx $(wildcard *.ml) $(wildcard *.mli)
	$(OCB) $(OCB_FLAGS) $@

core.docdir/index.html: 
	$(OCB) $(OCB_FLAGS) $@

math_util.cmx: math_util.ml math_util.mli
	$(OCB) $(OCB_FLAGS)

cstub_test: c_test.c closedform.c closedform.h
	gcc $^ -o $@ -lm

clean:
	$(OCB) -clean

.PHONY: clean
