include Makefile.common

all: dirs tests $S/codept-server $S/codept-client $S/codept

dirs: lib/ lib/sexp/ full/ tests/ docs/ precomputed/
	mkdir -p lib/sexp full tests docs precomputed

$S/codept: $(OBJ) full/codept.cmx
	$(OCAMLOPT) $(LKS) $(call tocmx, $(codeptSORT)) -o $S/codept

$S/codept-client: $(OBJ) full/codept_client.cmx
	$(OCAMLOPT) $(LKS) $(call tocmx, $(codept_clientSORT)) -o $S/codept-client


$S/codept-server: $(OBJ) full/codept_server.cmx
	$(OCAMLOPT) $(LKS) $(call tocmx, $(codept_serverSORT)) -o $S/codept-server


$S/test-run: $(OBJ) tests/run.cmx
	$(OCAMLOPT) $(LKS)  $(call tocmx, $(runSORT)) -o $S/test-run


$S/test-serialize: $(OBJ) tests/serialization.cmx
	$(OCAMLOPT) $(LKS) $(call tocmx, $(serializationSORT) ) -o $S/test-serialize

tests: $S/test-run $S/test-serialize
	cd $S && ./test-run && ./test-serialize


print-%  : ; @echo $* = $($*)

$(SRC:%.mli=%.cmi):%.cmi: $S/%.mli
	$(OCAMLC) -c $< -o $@

%.cmo: $S/%.ml %.cmi
	$(OCAMLC) -c $< -o $@

$(OBJ):%.cmx: $S/%.ml
	$(OCAMLOPT) -c $< -o $@

$(BGENS:=.cmx):%.cmx: %.ml %.cmi
	$(OCAMLOPT) -c $< -o $@

lib/sexp/sexp_parse.cmi: lib/sexp/sexp_parse.mli
	$(OCAMLC) -c $< -o $@

lib/sexp/sexp_lex.cmi: lib/sexp/sexp_lex.ml lib/sexp/sexp_parse.cmi
	$(OCAMLC) -c $< -o $@


clean:
	rm lib/*.cm[iox]; rm full/*.cm[iox]; rm precomputed/*.cm[iox]; rm lib/sexp/*cm[iox]


%.cmi: %.mli
	$(OCAMLC) -c $< -o $@


$(ODOC):docs/%.odoc: $S/lib/%.mli
	ocamldoc -I lib -I +compiler-libs $< -dump $@

doc: docs/index.html
docs/index.html: $(ODOC)
	ocamldoc $(addprefix -load , $(ODOC)) -html -charset utf-8 -d docs

include codept.sort codept_server.sort codept_client.sort run.sort serialization.sort .depends
