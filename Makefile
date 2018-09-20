OPTS= -use-ocamlfind -use-menhir
BUILD=build
include Makefile.config
S=$(abspath .)

ifeq ($(DUNE), enabled)
all: dune-all
clean: dune-clean
else
all: make-all
clean: make-clean
  ifeq ($(OCAMLBUILD), enabled)
  OCAMLBUILD=`ocamlbuild -where`
  all:codept_ocamlbuild
  endif
endif

alternative: lib/*.ml lib/*.mli full/*.ml full/*.mli precomputed/*.ml tests/*.ml
	make -C $(BUILD) -j

make-all:
	make -C build -j $S/codept

alt2-%:
	make -C build -j $*

dune-all:
	dune build @install

codept: lib/*.ml lib/*.mli full/*.ml full/*.mli precomputed/*.ml
	ocamlbuild $(OPTS) codept.native\
		&& mv codept.native codept

codept_ocamlbuild: ocamlbuild_plugin/codept_ocamlbuild.ml
	cd ocamlbuild_plugin \
	&& ocamlbuild -no-ocamlfind -cflags -I,$(OCAMLBUILD) \
	codept_ocamlbuild.otarget
dune-clean:
	dune clean
make-clean:
	rm codept
	cd build; make clean

.PHONY:tests
tests:
	dune runtest


codept-server: codept_server.native
	mv codept_server.native codept-server

codept-client: codept_client.native
	mv codept_client.native codept-client

doc:
	dune build @private-docs

ocamlbuild_self_test:
	ln -s ocamlbuild/myocamlbuild_cs.ml myocamlbuild.ml; \
	ocamlbuild $(OPTS) codept.native; \
	rm myocamlbuild.ml

self_ref: OPTS = -use-ocamlfind

