OPTS= -use-ocamlfind

all: codept codept-client codept-server test

codept: lib/*.ml lib/*.mli full/*.ml full/*.mli
	ocamlbuild $(OPTS) codept.native\
		&& mv codept.native codept

clean:
	ocamlbuild -clean

test: tests/**/*.ml test-run test-serialization codept
	./test-run && ./test-serialization

test-%: tests/%.ml codept
	ocamlbuild $(OPTS) $*.native && mv $*.native $@ 

%.native: full/%.ml
	ocamlbuild $(OPTS) $*.native 

codept-server: codept_server.native
	mv codept_server.native codept-server

codept-client: codept_client.native
	mv codept_client.native codept-client

doc: codept
	ocamlbuild $(OPTS) -docflags -charset,utf-8 codept.docdir/index.html

self_test:
	ln -s ocamlbuild/myocamlbuild.ml myocamlbuild.ml; \
	ocamlbuild $(OPTS) codept.native; \
	rm myocamlbuild.ml
