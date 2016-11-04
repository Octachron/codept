codept: lib/*.ml legacy/*.ml
	ocamlbuild -use-ocamlfind -package compiler-libs.common codept.native\
		&& mv codept.native codept

clean:
	ocamlbuild -clean
