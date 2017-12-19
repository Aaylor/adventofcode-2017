
## OCAML

OCAMLFIND = ocamlfind
OCAMLC = ocamlc
OCAMLOPT = ocamlopt


## BINARY

BINARY = solver
SOURCE = $(BINARY).ml
SOURCE_CMX = $(SOURCE:%.ml=%.cmx)


## RULES

$(BINARY): $(SOURCE_CMX)
	@$(OCAMLFIND) $(OCAMLOPT) -o $@ $^


## GENERIC RULES

%.cmx: %.ml
	@$(OCAMLFIND) $(OCAMLOPT) -c $<

%.cmi: %.mli
	@$(OCAMLFIND) $(OCAMLC) -c $<

.PHONY: clean
clean:
	@rm -f *.cm* *.o
	@rm -f $(BINARY)
