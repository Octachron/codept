
open Ocamlbuild_plugin


let mdeps = A "-nl-modules"
let fdeps = A "-modules"
let gen_sig = A "-sig"
let m2l_gen = A "-m2l"
let o = A "-o"

let is_pflag_included root s =
  let predicate t =
    match String.split_on_char '(' t with
    | a :: _ -> a = root
    | _ -> false in
  List.exists predicate @@ Tags.elements s


let codept' port  ~approx mode tags =
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
  let k = if approx then S [ A"-k"; A "-verbosity"; A "error" ]
    else S [] in
  let pp = Command.reduce @@ T (tags++ "ocaml" ++ "pp" ++ "pp:dep") in
  let pp = match pp with N -> N | s -> S [ A "-pp"; Quote s] in
  S [ A "codept-client"; A "-port"; A port; T tags'; pp ; k; mode]


let codept port ?(approx=true)  arg outs env _build =
  let arg = env arg in
  let tags = tags_of_pathname arg in
  let out = env @@ snd @@ List.hd outs in
  tag_file out (Tags.elements tags);
  let outs = List.map (fun (mode, name) -> S [o; Px (env name); mode ] ) outs in
  (** use codept "-k" to not fail on apparent self-reference *)
  Cmd(S[codept' port ~approx N tags; P arg; S outs])


let codept_dep port ?(approx=false) arg deps outs env build =
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
  Cmd( S[ codept' port ~approx (S[]) tags; P arg; Command.atomize_paths sigs;
          S outs])

module R() = struct

  let port =
    let socket = Unix.(socket PF_INET SOCK_STREAM 0) in
    let addr = Unix.( ADDR_INET (inet_addr_loopback, 0) ) in
    Unix.bind socket addr;
    Unix.listen socket 10;
    let port =   match Unix.getsockname socket with
      | Unix.ADDR_INET(_,port) -> port
      | Unix.ADDR_UNIX _  -> raise (Invalid_argument "no port for unix socket") in
    let _n =
      Unix.create_process "codept-server"
        [|"codept-server"; "-backport"; string_of_int port  |]
        Unix.stdin Unix.stdout Unix.stderr in
    let s, _ = Unix.accept socket in
    let chan = Unix.in_channel_of_descr s in
    let n : int = input_value chan in
    string_of_int n in

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
    (codept port  "%.ml" [m2l_gen, "%.m2l"; mdeps, "%.ml.approx.depends"] );

rule "mli → m2li + approx.depends"
  ~insert:`top
  ~prods:["%.m2li";"%.mli.approx.depends"]
  ~dep:"%.mli"
  ~doc:"Generate m2li files from mli files. See \"ml → m2li\" for more context"
  (codept port "%.mli" [m2l_gen, "%.m2li"; mdeps, "%.mli.approx.depends"])
;

rule "m2li → depends"
  ~insert:`top
  ~prods:["%.sig";"%.mli.depends"]
  ~deps:["%.m2li"; "%.mli.approx.depends"]
  ~doc:"Compute the signature and its signature dependency for the current module. \
        Note that signature dependency are a (hopefully small) subset of the the \
        full dependencies."
  (codept_dep port ~approx:false "%.m2li" "%.mli.approx.depends"
     [gen_sig, "%.sig"; fdeps, "%.mli.depends"]);

rule "m2l → depends"
  ~insert:(`after "m2li → depends")
  ~prods:["%.ml.depends"; "%.sig"]
  ~deps:["%.m2l"; "%.ml.approx.depends"]
  ~doc:"If there is no correspoding mli file, compute signature and associated \
        dependencies from the ml file."
  (codept_dep port ~approx:false
     "%.m2l" "%.ml.approx.depends" [gen_sig, "%.sig"; fdeps, "%.ml.depends"]);

rule "m2l → ml.depends"
  ~insert:(`before "m2l → depends")
  ~prods:["%.ml.depends"]
  ~deps:["%.m2l";"%.ml.approx.depends"]
  ~doc:"Compute the exact dependencies using codept and \
        the contextual information acquired following previously discovered \
        approximate dependencies."
  (codept_dep port "%.m2l" "%.ml.approx.depends"
     [fdeps, "%.ml.depends"] )
;

end

let () =
  dispatch(function
      | After_rules -> let module M = R() in ()
      | _ -> ()
    )
