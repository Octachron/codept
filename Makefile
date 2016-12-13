codept: lib/*.ml lib/*.mli light/*.ml
	ocamlbuild -use-ocamlfind codept.native\
		&& mv codept.native codept

clean:
	ocamlbuild -clean

test: tests/**/*.ml run.native serialization.native codept
	./run.native && ./serialization.native

%.native: tests/%.ml codept
	ocamlbuild -use-ocamlfind $@

doc: codept
	ocamlbuild -use-ocamlfind -docflags -charset,utf-8 codept.docdir/index.html
