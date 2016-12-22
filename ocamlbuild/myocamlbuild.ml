
open Ocamlbuild_plugin


let mdeps = A "-nl-modules"
let fdeps = A "-modules"
let sig_only =A "-sig-only"
let gen_sig = S[ A "-sig"; sig_only ]
let m2l_gen = A "-m2l-sexp"

let is_pflag_included root s =
  let predicate t =
    match String.split_on_char '(' t with
    | a :: _ -> a = root
    | _ -> false in
  List.exists predicate @@ Tags.elements s


let codept' mode tags =
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
  S [ A "codept"; T tags'; mode]

let codept mode arg out env _build =
  let arg = env arg and out = env out in
  let tags = tags_of_pathname arg in
  Cmd(S[codept' mode tags; P arg; Sh ">"; Px out])


let codept_dep mode arg deps out env build =
  let arg = env arg and out = env out and deps = env deps in
  let tags = tags_of_pathname arg in
  let approx_deps = string_list_of_file deps in
  let include_dirs = Pathname.(include_dirs_of @@ dirname arg ) in
  let sigs = List.map (fun m -> expand_module include_dirs m ["sig"])
      approx_deps in
  let outsigs = build sigs in
  let sigs =
    List.map Outcome.good
    @@ List.filter Outcome.(function Good _ -> true | Bad _ -> false )
    @@ outsigs in
  Cmd( S[ codept' mode tags; P arg; Command.atomize_paths sigs;
          Sh ">"; Px out])

module R() = struct
  let mdeps = A "-nl-modules" in
  let fdeps = A "-modules" in
  let sig_only = A "-sig-only" in
  let gen_sig = S [ A "-sig"; sig_only ] in
  let m2l_gen = A "-m2l-sexp" in

  rule "ml → m2l"
    ~insert:`top
    ~prod:"%.m2l"
    ~dep:"%.ml"
    (codept m2l_gen "%.ml" "%.m2l");

rule "mli → m2li"
  ~insert:`top
  ~prod:"%.m2li"
  ~dep:"%.mli"
  (codept m2l_gen "%.mli" "%.m2li")
;

rule "m2l → ml.r.depends"
  ~insert:`top
  ~prod:"%.ml.r.depends"
  ~dep:"%.m2l"
  ~doc:"Compute approximate dependencies using codept."
  (codept mdeps "%.ml" "%.ml.r.depends");

rule "m2li → mli.r.depends"
  ~insert:`top
  ~prod:"%.mli.r.depends"
  ~dep:"%.m2li"
  ~doc:"Compute approximate dependencies using codept."
  (codept mdeps "%.mli" "%.mli.r.depends");

rule "m2li → sig"
  ~insert:`top
  ~prod:"%.sig"
  ~deps:["%.m2li";"%.sig.depends"]
  ~doc:"Compute approximate dependencies using codept."
  (codept_dep gen_sig "%.m2li" "%.sig.depends"
     "%.sig");

rule "m2l → sig"
  ~insert:(`after "m2li → sig")
  ~prod:"%.sig"
  ~deps:["%.m2l";"%.sig.depends"]
  ~doc:"Compute approximate dependencies using codept."
  (codept_dep gen_sig
     "%.m2l" "%.sig.depends" "%.sig");

rule "m2li → r.sig.depends"
  ~insert:`top
  ~prod:"%.r.sig.depends"
  ~dep:"%.m2li"
  ~doc:"Compute approximate dependencies using codept."
  (codept (S [ mdeps; sig_only]) "%.m2li" "%.r.sig.depends");

rule "m2l → r.sig.depends"
  ~insert:(`after "m2li → r.sig.depends")
  ~prod:"%.r.sig.depends"
  ~dep:"%.m2l"
  ~doc:"Compute approximate dependencies using codept."
  (codept (S [ mdeps; sig_only]) "%.m2l" "%.r.sig.depends");


rule "m2li r.sig.depends → sig.depends"
  ~insert:`top
  ~prod:"%.sig.depends"
  ~deps:["%.m2li"; "%.r.sig.depends"]
  ~doc:"Compute approximate dependencies using codept."
  (codept_dep (S [ mdeps; sig_only])
                 "%.m2li" "%.r.sig.depends" "%.sig.depends")
;

rule "m2l r.sig.depends → sig.depends"
  ~insert:(`after "m2li r.sig.depends → sig.depends")
  ~prod:"%.sig.depends"
  ~deps:["%.m2l"; "%.r.sig.depends"]
  ~doc:"Compute approximate dependencies using codept."
  (codept_dep (S [ mdeps; sig_only])
     "%.m2l" "%.r.sig.depends" "%.sig.depends")
;


rule "m2l → depends"
  ~insert:`top
  ~prod:"%.ml.depends"
  ~deps:["%.m2l";"%.ml.r.depends"]
  ~doc:"Compute approximate dependencies using codept."
  (codept_dep fdeps "%.ml" "%.ml.r.depends" "%.ml.depends")
;


rule "m2li → depends"
  ~insert: `top
  ~prod:"%.mli.depends"
  ~deps:["%.m2li";"%.mli.r.depends"]
  (codept_dep fdeps "%.mli" "%.mli.r.depends" "%.mli.depends")

end

let () =
  dispatch(function
      | After_rules -> let module M = R() in ()
      | _ -> ()
    )
