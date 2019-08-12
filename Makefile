all:
	dune build @install


S=$(abspath .)

make-all:
	make -C build -j $S/codept

codept_ocamlbuild:
	dune build ocamlbuild_plugin/ocamlbuild_plugin.cma

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

