
open Ocamlbuild_plugin


let mdeps = A "-nl-modules"
let fdeps = A "-modules"
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
  let k = if approx then S [ A"-k"; A "-silent-fault-level"; A "warning" ]
    else S [] in
  let pp = Command.reduce @@ T (tags++ "ocaml" ++ "pp" ++ "pp:dep") in
  let pp = match pp with N -> N | s -> S [ A "-pp"; Quote s] in
  S [ A "codept-client"; T tags'; pp ; k; mode]


let codept ?(approx=true)  arg outs env _build =
  let arg = env arg in
  let tags = tags_of_pathname arg in
  let out = env @@ snd @@ List.hd outs in
  tag_file out (Tags.elements tags);
  let outs = List.map (fun (mode, name) -> S [o; Px (env name); mode ] ) outs in
  (** use codept "-k" to not fail on apparent self-reference *)
  Cmd(S[codept' ~approx N tags; P arg; S outs])


let codept_dep ?(approx=false) arg deps outs env build =
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
  Cmd( S[ codept' ~approx (S[]) tags; P arg; Command.atomize_paths sigs;  S outs])

module R() = struct


  ignore @@
    Unix.create_process "codept-server" [|"codept-server"|] Unix.stdin Unix.stdout
      Unix.stderr
;

  rule "ml → m2l + approx.depends"
    ~insert:`top
    ~prods:["%.m2l";"%.ml.approx.depends"]
    ~dep:"%.ml"
    ~doc:"Generate m2l files from ml files. This conversion has two distinct \
          objectives: \n\
          − normalize the input file to avoid unnecessary dependency \
          recomputation when the underlying ml file changes does not affect the \
          m2l level \n\
          − avoid multiple parsing of the same file, in particular in presence of \
          heavy preprocessor rewriting"
    (codept  "%.ml" [m2l_gen, "%.m2l"; mdeps, "%.ml.approx.depends"] );

rule "mli → m2li + approx.depends"
  ~insert:`top
  ~prods:["%.m2li";"%.mli.approx.depends"]
  ~dep:"%.mli"
  ~doc:"Generate m2li files from mli files. See \"ml → m2li\" for more context"
  (codept "%.mli" [m2l_gen, "%.m2li"; mdeps, "%.mli.approx.depends"])
;

rule "m2li → depends"
  ~insert:`top
  ~prods:["%.sig";"%.mli.depends"]
  ~deps:["%.m2li"; "%.mli.approx.depends"]
  ~doc:"Compute the signature and its signature dependency for the current module. \
        Note that signature dependency are a (hopefully small) subset of the the \
        full dependencies."
  (codept_dep ~approx:false "%.m2li" "%.mli.approx.depends"
     [gen_sig, "%.sig"; fdeps, "%.mli.depends"]);

rule "m2l → depends"
  ~insert:(`after "m2li → depends")
  ~prods:["%.ml.depends"; "%.sig"]
  ~deps:["%.m2l"; "%.ml.approx.depends"]
  ~doc:"If there is no correspoding mli file, compute signature and associated \
        dependencies from the ml file."
  (codept_dep ~approx:false
     "%.m2l" "%.ml.approx.depends" [gen_sig, "%.sig"; fdeps, "%.ml.depends"]);

rule "m2l → ml.depends"
  ~insert:(`before "m2l → depends")
  ~prods:["%.ml.depends"]
  ~deps:["%.m2l";"%.ml.approx.depends"]
  ~doc:"Compute the exact dependencies using codept and \
        the contextual information acquired following previously discovered \
        approximate dependencies."
  (codept_dep "%.m2l" "%.ml.approx.depends"
     [fdeps, "%.ml.depends"] )
;

end

let () =
  dispatch(function
      | After_rules -> let module M = R() in ()
      | _ -> ()
    )
