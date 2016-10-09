adept: lib/*.ml legacy/*.ml
	ocamlbuild -use-ocamlfind -package compiler-libs.common main.native\
		&& mv main.native adept

clean:
	ocamlbuild -clean
