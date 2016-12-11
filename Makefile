codept: lib/*.ml lib/*.mli light/*.ml
	ocamlbuild -use-ocamlfind codept.native\
		&& mv codept.native codept

clean:
	ocamlbuild -clean

test: tests/**/*.ml codept
	ocamlbuild -use-ocamlfind run.native \
	&& ocamlbuild -use-ocamlfind sig_reader.native \
	&& ./run.native && ./sig_reader.native

doc: codept
	ocamlbuild -use-ocamlfind -docflags -charset,utf-8 codept.docdir/index.html
