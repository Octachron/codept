codept: lib/*.ml lib/*.mli light/*.ml
	ocamlbuild -use-ocamlfind codept.native\
		&& mv codept.native codept

clean:
	ocamlbuild -clean

test: tests/**/*.ml codept
	ocamlbuild -use-ocamlfind run.native\
	&& ./run.native

doc: codept
	ocamlbuild -use-ocamlfind codept.docdir/index.html
