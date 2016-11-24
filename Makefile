codept: lib/*.ml lib/*.mli light/*.ml
	ocamlbuild -use-ocamlfind codept.native\
		&& mv codept.native codept

clean:
	ocamlbuild -clean
