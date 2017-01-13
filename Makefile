all:
	ocamlbuild huffman.o
	ocamlbuild -use-ocamlfind -package qcheck huffmanTest.byte -lflags -custom,huffman.o

clean:
	rm -rf _build
	rm -f huffmanTest.byte
