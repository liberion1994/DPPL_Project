# Makefile
build:
	ocamlbuild -use-ocamlfind main.d.byte
clean:
	ocamlbuild -clean
test:
	./main.d.byte test.f

.PHONY: build clean
