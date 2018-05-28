all: o-bigarray o-num o-threads o-dynlink o-graph o-unix stdlib

o-%:
	../codept -o std_$*.ml -export stdlib/$* $(OCAMLSRC)/otherlibs/$*/*.mli

stdlib:
	../codept -o stdlib.ml -export stdlib_data $(OCAMLSRC)/stdlib/*.mli


