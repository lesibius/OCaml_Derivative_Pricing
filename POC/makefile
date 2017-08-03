OCB = ocamlbuild
OCB_FLAGS = -use-ocamlfind -tags thread

all: test.native

%.native:$(wildcard *.ml)
	$(OCB) $(OCB_FLAGS) $@

clean:
	$(OCB) -clean

.PHONY: clean all
