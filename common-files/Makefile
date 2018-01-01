
## UTILS

HEAD = printf "%-10s %s\n"


## OCAML

OCAMLFIND = ocamlfind
OCAMLC = ocamlc
OCAMLOPT = ocamlopt


## BINARY

BINARY = solver
SOURCE = $(BINARY).ml
SOURCE_CMX = $(SOURCE:%.ml=%.cmx)


## LIBRARIES

LIBRARY_FOLDER = common-libs
LIBRARY_SOURCES = $(LIBRARY_FOLDER)/aoc_lib.ml \
	$(LIBRARY_FOLDER)/aoc_solver.ml
LIBRARY_CMX = $(LIBRARY_SOURCES:%.ml=%.cmx)
LIBRARY_CMI = $(LIBRARY_SOURCES:%.ml=%.cmi)


## RULES

$(BINARY): $(LIBRARY_CMX) $(SOURCE_CMX)
	@$(OCAMLFIND) $(OCAMLOPT) -package unix -linkpkg -I $(LIBRARY_FOLDER) -o $@ $^

run: $(BINARY)
	@./$(BINARY)

## GENERIC RULES

$(LIBRARY_CMX): $(LIBRARY_CMI)

%.cmx: %.ml
	@$(HEAD) "Ocamlopt" "$<"
	@$(OCAMLFIND) $(OCAMLOPT) -package unix -linkpkg -I $(LIBRARY_FOLDER) -c $<

%.cmi: %.mli
	@$(HEAD) "Ocamlc" "$<"
	@$(OCAMLFIND) $(OCAMLC) -c -I $(LIBRARY_FOLDER) $<

.PHONY: clean
clean:
	@rm -f *.cm* *.o
	@rm -f $(LIBRARY_FOLDER)/*.cm* $(LIBRARY_FOLDER)/*.o
	@rm -f $(BINARY)
