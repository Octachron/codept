fdep: src/*.ml
	ocamlbuild -use-ocamlfind -package compiler-libs.common main.native\
	&& mv main.native fdep

clean:
	ocamlbuild -clean
