OPTS= -use-ocamlfind -use-menhir
BUILD=build

all:
	dune build @install


include Makefile.config
S=$(abspath .)

make-all:
	make -C build -j $S/codept

alt2-%:
	make -C build -j $*

codept: lib/*.ml lib/*.mli full/*.ml full/*.mli bundled/*.ml bundled/*.mli
	ocamlbuild $(OPTS) codept.native\
		&& mv codept.native codept

codept_ocamlbuild: ocamlbuild_plugin/codept_ocamlbuild.ml
	cd ocamlbuild_plugin \
	&& ocamlbuild -no-ocamlfind -cflags -I,$(OCAMLBUILD) \
	codept_ocamlbuild.otarget

clean:
	dune clean

make-clean:
	rm codept
	cd build; make clean

.PHONY:tests
tests:
	dune runtest

doc:
	dune build @private-docs

ocamlbuild_self_test:
	ln -s ocamlbuild/myocamlbuild_cs.ml myocamlbuild.ml; \
	ocamlbuild $(OPTS) codept.native; \
	rm myocamlbuild.ml

self_ref: OPTS = -use-ocamlfind

