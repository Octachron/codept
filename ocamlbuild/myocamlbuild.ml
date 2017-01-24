
open Ocamlbuild_plugin


let mdeps = A "-nl-modules"
let fdeps = A "-modules"
let sig_only =A "-sig-only"
let gen_sig = A "-sig"
let m2l_gen = A "-m2l-sexp"
let o = A "-o"

let is_pflag_included root s =
  let predicate t =
    match String.split_on_char '(' t with
    | a :: _ -> a = root
    | _ -> false in
  List.exists predicate @@ Tags.elements s


let codept' ~approx mode tags =
  let tags' = tags++"ocaml"++"ocamldep" ++ "codept" in
  let tags' =
    (* when computing dependencies for packed modules, cyclic alias
       references within the packed modules becomes problematic when
       packing. To avoid creating packed cmo/cmx with invalid elements,
       we disallow the simultaneous use of no_alias_deps and for-pack(…). *)
    if is_pflag_included "for-pack" tags' then
      tags' -- "no_alias_deps"
    else
      tags' in
  let k = if approx then S [ A"-k"; A "-silent-fault-level"; A "notification" ]
    else S [] in
  let pp = Command.reduce @@ T (tags++ "ocaml" ++ "pp" ++ "pp:dep") in
  let pp = match pp with N -> N | s -> S [ A "-pp"; Quote s] in
  S [ A "codept-client"; T tags'; pp ; k; mode]


let codept ?(approx=true) mode arg out env _build =
  let arg = env arg and out = env out in
  let tags = tags_of_pathname arg in
  tag_file out (Tags.elements tags);
  (** use codept "-k" to not fail on apparent self-reference *)
  Cmd(S[codept' ~approx mode tags; P arg; Sh ">"; Px out])


let codept_dep ?(approx=false) mode arg deps outs env build =
  let arg = env arg  and deps = env deps in
  let tags = tags_of_pathname arg in
  let approx_deps = string_list_of_file deps in
  (** eliminate self-dependency *)
  let approx_deps =
    List.filter (fun x -> x <> module_name_of_pathname arg) approx_deps in
  let include_dirs = Pathname.(include_dirs_of @@ dirname arg ) in
  let sigs = List.map (fun m -> expand_module include_dirs m ["sig"])
      approx_deps in
  let outsigs = build sigs in
  let sigs =
    List.map Outcome.good
    @@ List.filter Outcome.(function Good _ -> true | Bad _ -> false )
    @@ outsigs in
  let outs = List.map (fun (mode, name) -> S [o; Px (env name); mode ] ) outs in
  Cmd( S[ codept' ~approx mode tags; P arg; Command.atomize_paths sigs;  S outs])

module R() = struct


  ignore @@
    Unix.create_process "codept-server" [|"codept-server"|] Unix.stdin Unix.stdout
      Unix.stderr
;

  rule "ml → m2l"
    ~insert:`top
    ~prod:"%.m2l"
    ~dep:"%.ml"
    ~doc:"Generate m2l files from ml files. This conversion has two distinct \
          objectives: \n\
          − normalize the input file to avoid unnecessary dependency \
          recomputation when the underlying ml file changes does not affect the \
          m2l level \n\
          − avoid multiple parsing of the same file, in particular in presence of \
          heavy preprocessor rewriting"
    (codept m2l_gen "%.ml" "%.m2l");

rule "mli → m2li"
  ~insert:`top
  ~prod:"%.m2li"
  ~dep:"%.mli"
  ~doc:"Generate m2li files from mli files. See \"ml → m2li\" for more context"
  (codept m2l_gen "%.mli" "%.m2li")
;

rule "m2l → ml.approx.depends"
  ~insert:`top
  ~prod:"%.ml.approx.depends"
  ~dep:"%.m2l"
  ~doc:"Compute approximate dependencies using codept: at this stage, contextual \
       is not yet available, and computed dependencies are used to gather \
        signature information on the potential dependencies."
  (codept mdeps "%.m2l" "%.ml.approx.depends");

rule "m2li → mli.approx.depends"
  ~insert:`top
  ~prod:"%.mli.approx.depends"
  ~dep:"%.m2li"
  ~doc:"Compute approximate dependencies using codept. \
        See \"m2l → ml.approx.depends\" for more context."
  (codept mdeps "%.m2li" "%.mli.approx.depends");

rule "m2li → sig depends"
  ~insert:`top
  ~prods:["%.sig";"%.sig.depends"]
  ~deps:["%.m2li"; "%.approx.sig.depends"]
  ~doc:"Compute the signature and its signature dependency for the current module. \
        Note that signature dependency are a (hopefully small) subset of the the \
        full dependencies."
  (codept_dep ~approx:false sig_only "%.m2li" "%.approx.sig.depends"
     [gen_sig, "%.sig"; fdeps, "%.sig.depends"]);

rule "m2l → sig depends"
  ~insert:(`after "m2li → sig depends")
  ~prods:["%.sig"; "%.sig.depends"]
  ~deps:["%.m2l"; "%.approx.sig.depends"]
  ~doc:"If there is no correspoding mli file, compute signature and associated \
        dependencies from the ml file."
  (codept_dep ~approx:false sig_only
     "%.m2l" "%.approx.sig.depends" [gen_sig, "%.sig"; fdeps, "%.sig.depends"]);

rule "m2li → approx.sig.depends"
  ~insert:`top
  ~prod:"%.approx.sig.depends"
  ~dep:"%.m2li"
  ~doc:"Compute approximate dependencies for the signature computation. \
        Signature level dependencies are the subset of the full dependencies \
        required to compute the compilation unit signature. In particular, if \
        a compilation unit does not contain include nor submodule, then \
        signature dependencies are reduced to the empty set."
  (codept (S [ mdeps; sig_only]) "%.m2li" "%.approx.sig.depends");

rule "m2l → approx.sig.depends"
  ~insert:(`after "m2li → approx.sig.depends")
  ~prod:"%.approx.sig.depends"
  ~dep:"%.m2l"
  ~doc:"If there is no mli associated to a given computation unit, \
        compute the approximate signature dependencies from the ml file."
  (codept (S [ mdeps; sig_only]) "%.m2l" "%.approx.sig.depends");


rule "m2l → ml.depends"
  ~insert:`top
  ~prods:["%.ml.depends"]
  ~deps:["%.m2l";"%.ml.approx.depends"]
  ~doc:"Compute the exact dependencies using codept and \
        the contextual information acquired following previously discovered \
        approximate dependencies."
  (codept_dep N "%.m2l" "%.ml.approx.depends"
     [fdeps, "%.ml.depends"] )
;


rule "m2li → mli.depends"
  ~insert: `top
  ~prods:["%.mli.depends"]
  ~deps:["%.m2li";"%.mli.approx.depends"]
  ~doc:"Compute the exact dependencies using codept and \
        the contextual information acquired following previously discovered \
        approximate dependencies."
  (codept_dep N "%.m2li" "%.mli.approx.depends"
     [fdeps, "%.mli.depends"] )

end

let () =
  dispatch(function
      | After_rules -> let module M = R() in ()
      | _ -> ()
    )
