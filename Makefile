adept: lib/*.ml legacy/*.ml
	ocamlbuild -use-ocamlfind -package compiler-libs.common main.native\
		&& mv main.native codept

clean:
	ocamlbuild -clean
