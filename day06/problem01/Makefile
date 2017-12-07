
BIN=memory

all: $(BIN)

$(BIN): $(BIN).ml
	ocamlopt $^ -o $@

clean:
	rm -f *.cm* $(BIN) *.o
