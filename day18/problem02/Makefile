
BIN=duet

all: $(BIN)

$(BIN): $(BIN).ml
	ocamlfind ocamlopt -package lwt,lwt.ppx $^ -o $@

clean:
	rm -f *.cm* $(BIN) *.o
