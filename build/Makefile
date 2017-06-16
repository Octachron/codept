include Makefile.common

all: dirs tests $S/codept-server $S/codept-client $S/codept

dirs:
	mkdir -p full lib/sexp tests docs precomputed

$S/codept: $(OBJ) full/codept.cmx | dirs
	@$(OCAMLOPT) $(LKS) $(call tocmx, $(codeptSORT)) -o $S/codept

$S/codept-client: $(OBJ) full/codept_client.cmx | dirs
	@$(OCAMLOPT) $(LKS) $(call tocmx, $(codept_clientSORT)) -o $S/codept-client


$S/codept-server: $(OBJ) full/codept_server.cmx | dirs
	@$(OCAMLOPT) $(LKS) $(call tocmx, $(codept_serverSORT)) -o $S/codept-server


$S/test-run: $(OBJ) tests/run.cmx
	@$(OCAMLOPT) $(LKS)  $(call tocmx, $(runSORT)) -o $S/test-run


$S/test-serialize: $(OBJ) tests/serialization.cmx
	@$(OCAMLOPT) $(LKS) $(call tocmx, $(serializationSORT) ) -o $S/test-serialize

test: $S/test-run $S/test-serialize
	@cd $S && ./test-run && ./test-serialize


print-%  : ; @echo $* = $($*)

$(SRC:%.mli=%.cmi):%.cmi: $S/%.mli | dirs
	@$(OCAMLC) -c $< && mv $S/$@ $@ && mv $S/$*.cmti $*.cmti

%.cmo: $S/%.ml %.cmi | dirs
	@$(OCAMLC) -c $< && mv $S/$@ $@ && mv $S/$*.cmt $*.cmt

$(OBJ):%.cmx: $S/%.ml | dirs
	@$(OCAMLOPT) -c $< && mv $S/$@ $@ && mv $S/$*.o $*.o && mv $S/$*.cmt $*.cmt && (mv $S/$*.cmi $*.cmi 2> /dev/null ||true)
$(BGENS:=.cmx):%.cmx: %.ml %.cmi
	@$(OCAMLOPT) -c $<

lib/sexp/sexp_parse.cmi: lib/sexp/sexp_parse.mli
	@$(OCAMLC) -c $<

lib/sexp/sexp_lex.cmi: lib/sexp/sexp_lex.ml lib/sexp/sexp_parse.cmi
	@$(OCAMLC) -c $<


clean:
	@rm lib/*.cm[iox] lib/*.cmt lib/*.cmti lib/sexp/*.cm[iox];\
	rm -R full; rm -R precomputed


%.cmi: %.mli | dirs
	@$(OCAMLC) -c $< && mv $S/$@ $@


$(ODOC):docs/%.odoc: $S/lib/%.mli | dirs
	@ocamldoc -I lib -I +compiler-libs $< -dump $@

docs: $S/docs/index.html | dirs
$S/docs/index.html: $(ODOC)
	@ocamldoc $(addprefix -load , $(ODOC)) -html -charset utf-8 -d $S/docs

include codept.sort codept_server.sort codept_client.sort run.sort serialization.sort .depends
