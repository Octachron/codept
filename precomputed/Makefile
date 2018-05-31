all: o-bigarray o-num o-threads o-dynlink o-graph o-unix stdlib prefixed_stdlib

o-%:
	../codept -o std_$*.ml -export $*  stdlib/$* $(OCAMLSRC)/otherlibs/$*/*.mli

stdlib:
	../codept -o stdlib_data.ml -export stdlib $(OCAMLSRC)/stdlib/*.mli

prefixed_stdlib:
	../codept -no-alias-deps -o prefixed_stdlib.ml -export prefixed_stdlib \
		$(OCAMLSRC)/stdlib/*.cmi



