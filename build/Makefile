include Makefile.common

all: dirs tests $S/codept-server $S/codept-client $S/codept

dirs:
	mkdir -p lib/sexp full tests docs precomputed

$S/codept: $(OBJ) full/codept.cmx dirs
	@$(OCAMLOPT) $(LKS) $(call tocmx, $(codeptSORT)) -o $S/codept

$S/codept-client: $(OBJ) full/codept_client.cmx dirs
	@$(OCAMLOPT) $(LKS) $(call tocmx, $(codept_clientSORT)) -o $S/codept-client


$S/codept-server: $(OBJ) full/codept_server.cmx dirs
	@$(OCAMLOPT) $(LKS) $(call tocmx, $(codept_serverSORT)) -o $S/codept-server


$S/test-run: $(OBJ) tests/run.cmx
	@$(OCAMLOPT) $(LKS)  $(call tocmx, $(runSORT)) -o $S/test-run


$S/test-serialize: $(OBJ) tests/serialization.cmx
	@$(OCAMLOPT) $(LKS) $(call tocmx, $(serializationSORT) ) -o $S/test-serialize

tests: $S/test-run $S/test-serialize dirs
	@cd $S && ./test-run && ./test-serialize


print-%  : ; @echo $* = $($*)

$(SRC:%.mli=%.cmi):%.cmi: $S/%.mli
	@$(OCAMLC) -c $< && mv $S/$@ $@ && mv $S/$*.cmti $*.cmti

%.cmo: $S/%.ml %.cmi
	@$(OCAMLC) -c $< && mv $S/$@ $@ && mv $S/$*.cmt $*.cmt

$(OBJ):%.cmx: $S/%.ml
	@$(OCAMLOPT) -c $< && mv $S/$@ $@ && mv $S/$*.o $*.o && mv $S/$*.cmt $*.cmt && (mv $S/$*.cmi $*.cmi 2> /dev/null ||true)
$(BGENS:=.cmx):%.cmx: %.ml %.cmi
	@$(OCAMLOPT) -c $<

lib/sexp/sexp_parse.cmi: lib/sexp/sexp_parse.mli
	@$(OCAMLC) -c $<

lib/sexp/sexp_lex.cmi: lib/sexp/sexp_lex.ml lib/sexp/sexp_parse.cmi
	@$(OCAMLC) -c $<


clean:
	@rm lib/*.cm[iox] ; rm full/*.cm[iox]; rm precomputed/*.cm[iox]; rm lib/sexp/*cm[iox] || true


%.cmi: %.mli
	@$(OCAMLC) -c $< && mv $S/$@ $@


$(ODOC):docs/%.odoc: $S/lib/%.mli
	@ocamldoc -I lib -I +compiler-libs $< -dump $@

docs: $S/docs/index.html dirs
$S/docs/index.html: $(ODOC)
	@ocamldoc $(addprefix -load , $(ODOC)) -html -charset utf-8 -d $S/docs

include codept.sort codept_server.sort codept_client.sort run.sort serialization.sort .depends
