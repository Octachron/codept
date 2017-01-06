
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
  S [ A "codept"; T tags'; k; mode]


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


let parse_maps file =
  match string_list_of_file file with
  |  [] | _ :: _ :: _ -> None
  | [a] ->
    let l = String.split_on_char ';' a in
    let l = List.map (fun x -> List.hd @@ String.split_on_char '{' x ) l in
    Some l

module R() = struct

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
  (codept mdeps "%.m2l" "%.ml.r.depends");

rule "m2li → mli.r.depends"
  ~insert:`top
  ~prod:"%.mli.r.depends"
  ~dep:"%.m2li"
  ~doc:"Compute approximate dependencies using codept."
  (codept mdeps "%.m2li" "%.mli.r.depends");

rule "m2li → sig depends"
  ~insert:`top
  ~prods:["%.sig";"%.sig.depends"]
  ~deps:["%.m2li"; "%.r.sig.depends"]
  ~doc:"Compute approximate dependencies using codept."
  (codept_dep ~approx:false sig_only "%.m2li" "%.r.sig.depends"
     [gen_sig, "%.sig"; fdeps, "%.sig.depends"]);

rule "m2l → sig depends"
  ~insert:(`after "m2li → sig depends")
  ~prods:["%.sig"; "%.sig.depends"]
  ~deps:["%.m2l"; "%.r.sig.depends"]
  ~doc:"Compute approximate dependencies using codept."
  (codept_dep ~approx:false sig_only
     "%.m2l" "%.r.sig.depends" [gen_sig, "%.sig"; fdeps, "%.sig.depends"]);

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


rule "m2l → ml.depends"
  ~insert:`top
  ~prods:["%.ml.depends"]
  ~deps:["%.m2l";"%.ml.r.depends"]
  ~doc:"Compute approximate dependencies using codept."
  (codept_dep N "%.m2l" "%.ml.r.depends"
     [fdeps, "%.ml.depends"] )
;


rule "m2li → mli.depends"
  ~insert: `top
  ~prods:["%.mli.depends"]
  ~deps:["%.m2li";"%.mli.r.depends"]
  (codept_dep N "%.m2li" "%.mli.r.depends"
     [fdeps, "%.mli.depends"] )

end

let () =
  dispatch(function
      | After_rules -> let module M = R() in ()
      | _ -> ()
    )
