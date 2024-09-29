open Params
module Cmd = Arg

type action = {
  modes:  (string * Modes.t)  list;
  singles: (string * Single.t) list;
  makefiles : string list
}

let action0 = { modes = []; singles = []; makefiles = [] }

let synonyms =
  let open Common in
  let add = Name.Map.add in
  Name.Map.empty
  |> add "ml" {kind=Implementation; format = Src}
  |> add "mli" {kind=Interface; format = Src}
  |> add "cmi" {kind=Interface; format = Cmi}
  |> add "m2l" {kind=Implementation; format = M2l}
  |> add "m2li" {kind=Interface; format = M2l}
  |> add "sig" {kind = Signature; format = Src }

let output = ref "%"
let param0 = {

    makefile = {
      all = false;
      native = false;
      bytecode = false;
      abs_path = false;
      slash = Filename.dir_sep;
      implicits = true;
      one_line = true;
      shared = false;
      includes = [];
    };

    synonyms;

    analyzer = {
      epsilon_dependencies = false;
      transparent_aliases = false;
      transparent_extension_nodes = true;
      precomputed_libs = Name.Set.singleton "stdlib";
      closed_world = false;
      sig_only = false;
      policy = Codept_policies.policy;
    };

    no_include = false;
    may_approx = false;
    nested = false;
    pretty_format = true;
    internal_format = Sexp;
    external_format = Json
  }

let task0 : Common.task = {
    Common.files = [];
    seeds = [];
    invisibles = Namespaced.Set.empty;
    libs = [];
    opens = [];
  }
let findlib_query0 =  Findlib.empty

let makefile_c action () =
  action :=
    { !action with makefiles = !output :: (!action).makefiles }

let with_output out s f=
  if s = "%" then
    f out
  else
    let chan= open_out s in
    let formatter= Format.formatter_of_out_channel chan in
    f formatter ;
    Format.pp_print_flush formatter ();
    close_out chan

let iter_makefile out param interm s =
  with_output out s (fun ppf ->
      Makefile.main L.(param#.policy) ppf param.synonyms param.makefile interm
    )

(** {2 Option implementations } *)
let first_ppx = Compenv.first_ppx

let add_ppx ppx =
  first_ppx := ppx :: !first_ppx

let ((#.),(#!),(#<-), fmap) = L.((#.),(#!),(#<-),fmap)

let ml_synonym param s =
  let info = { Common.format = Src; kind = Implementation } in
  fmap param L.synonyms (Name.Map.add s info)

let mli_synonym param s =
  let info = { Common.format = Src; kind = Interface } in
  fmap param L.synonyms (Name.Map.add s info)

let add_include param filename =
  fmap param L.includes (List.cons filename)

let eval_single out writer param (task:Common.task) (file,single) =
  with_output out file (fun ppf ->
      List.iter (Single.eval single file writer ppf param) task.files)

let iter_mode out writer param r (file,mode) =
  with_output out file (fun ppf ->
      Modes.eval mode file writer  ppf param r
    )

let format param x =
  let lenses = [L.inner_fmt;L.ext_fmt] in
  let update x = List.iter(fun l -> param#<-(l,x) ) lenses in
  match x with
    | "sexp" -> update Schematic.Sexp
    | "json" -> update Schematic.Json
    | "simple-json" -> param#<-(L.pretty_fmt,false); update Schematic.Json
    | _ -> ()


let mode action command () =
  let output = !output in
  action  :=
    { !action with
      modes = (output, command) :: (!action).modes
    }

let set_iter action command () =
  action :=
    { !action with
      singles =
        ( !output, command ) :: (!action).singles
    }

let print_vnum version ()= Format.printf "%.2f@." version
let print_version version ()= Format.printf "codept, version %.2f@." version

let fault param s =
  match Support.split_on_char '=' s with
  | [] | [_]| _ :: _ :: _ :: _ -> ()
  | [a;b] ->
    let path= List.map String.trim @@ Support.split_on_char '.' a in
    let level = Fault.Level.of_string b in
    fmap param L.policy (Fault.Policy.set ~lvl:level path)

let silent_level param s =
  fmap param L.policy (Fault.Policy.set_silent @@ Fault.Level.of_string s)

let exit_level param s =
  fmap param L.policy (Fault.Policy.set_exit @@ Fault.Level.of_string s)

let print_policy param ()=
  Fault.Policy.pp Pp.std param#!L.policy


let large_output ppf =
  Format.pp_set_margin ppf 160; Format.pp_set_max_indent ppf 120

let print_json_schema  =
  function
  | "sig" -> large_output Pp.std; Schematic.Ext.json_schema Pp.std Schema.namespace
  | "m2l" -> large_output Pp.std; Schematic.Ext.json_schema Pp.std Schema.m2l
  | "deps" -> large_output Pp.std; Schematic.Ext.json_schema Pp.std Schema.x
  | _ -> ()

let set_p param lens value =
  Cmd.Unit( fun () -> param#<-(lens, value))

let set_t param lens = set_p param lens true
let set_f param lens = set_p param lens false


let use_p param lens value =
  param#<-(lens, value)

let task_p param task f = Cmd.String (f param task)
let taskc task f = Cmd.String (f task)

let findlib findlib_query update =
  Cmd.String (fun s -> findlib_query := update !findlib_query s)

let pkg param findlib_query =
  let add s =
    if Common.is_stdlib_pkg s then
      fmap param L.precomputed_libs (Name.Set.add s)
    else
      findlib_query := Findlib.pkg !findlib_query s in
  Cmd.String( fun s -> List.iter add @@ Support.split_on_char ',' s )

let no_stdlib param =
  Cmd.Unit( fun () -> fmap param L.precomputed_libs @@ Name.Set.remove "stdlib" )

let usage_msg =
  "Codept is an alternative dependency solver for OCaml.\n\
   Usage: codept [options] [⟨signature files⟩] [⟨source files|m2l files⟩ groups]\n\
   − ⟨m2l⟩ files are serialized m2l ast files, identified by either a ⟨.m2l⟩ or\
     ⟨.m2li⟩ extension.\n\
   − ⟨signature⟩ files are signature information files, identified by \
   a ⟨.sig⟩ extension.\n\
   These two files format are useful to save persistent information between \
   multiple calls to codept.\n\n\
   — files can be submitted as file groups. File groups allows to easily isolate \
    a subset of files in a separated namespace. For instance, in \
    ⟨codept A[a.ml,b.ml] c.ml⟩ \
    the compilation units ⟨a.ml⟩ and ⟨b.ml⟩ would be mapped to ⟨A.A⟩ and ⟨B.B⟩.\
   More precisely, a file group consists in a non-empty comma-separated list \
   of file elements. \
   A file element consists either in a single name or in a subgroup.\
   This subgroup must be enclosed by ⟨[⟩ and ⟨]⟩ and contain itself a \
   non-empty comma-separated list of file elements. \
   Optionally, a subgroup can be prefixed by a module path, prefixing the \
   path of all elements inside the group with this prefix. \
   — Non-existent files and files with an unknown extension are ignored.\
   \n\
   The following options are common with ocamldep:\n"
open L

let args action param task fquery version =
  let set_t = set_t param in
  let set_f = set_f param in
  let use_p x =use_p param x in
  let set_p x = set_p param x in
  let task_p = task_p param task in
  let taskc = taskc task in
  let mode = mode action in
  let set_iter = set_iter action in
  let findlib = findlib fquery in

  Cmd.[
    "-absname", set_t abs_path, ": use absolute path name";
    "-all", set_t all, ": display full dependencies in makefile";
    "-allow-approx", set_p policy Codept_policies.parsing_approx,
    ": fall back to approximated parser \
                                        in presence of syntax errors.";
    "-as-map", task_p Task.as_map, "<file>: same as \
                                   \"-no-alias-deps <file>\"";
    "-I", String (add_include param),
    "<dir>: do not filter files in <dir> when printing dependencies";
    "-impl", taskc (Task.add_impl Src), "<f>: read <f> as a ml file";
    "-intf", taskc (Task.add_intf Src), "<f>: read <f> as a mli file";
    "-map", task_p Task.map, "<file>: same as \"-no-alias-deps \
                            -see <file>\"";
    "-ml-synonym", String (ml_synonym param),
    "<s>: use <s> extension as a synonym for ml";
    "-mli-synonym", String (mli_synonym param),
    "<s>: use <s> extension as a synonym for mli";
    "-modules", Unit (mode @@ Modes.(Modules(Standard, Dep))),
    ": print raw module dependencies";
    "-native", set_t native, ": generate native compilation only dependencies";
    "-bytecode", set_t bytecode, ": generate bytecode only dependencies";

    "-one-line", set_f one_line,
    ": output makefile dependencies on a single line for each target";
    "-open", taskc Task.add_open,
    "<name>: open module <name> at the start of all compilation units \
     (except units whose name is <name>).";
    "-pp", Cmd.String(fun s -> Clflags.preprocessor := Some s),
    "<cmd>: pipe sources through preprocessor <cmd>";
    "-ppx", Cmd.String add_ppx,
    "<cmd>: pipe abstract syntax trees through ppx preprocessor <cmd>";
    "-shared", set_t shared,
    ": generate dependencies for native plugin files (.cmxs) in addition to \
     native object files (.cmx).";
    "-slash", set_p slash "/", ": use forward slash as directory separator";
    "-sort", Unit(mode Modes.Sort),": sort files according to their dependencies";
    "-version", Cmd.Unit (print_version version),
    ": print human-friendly version description";
    "-vnum", Cmd.Unit (print_vnum version),
    ": print version number\n\n Major options:\n";

    "-expand-deps", set_t epsilon_dependencies,
    ": compute exact dependencies, rather than a subset of dependencies that is \
     equivalent to the exact dependency set up to transitive closure";
    "-k", set_p policy Codept_policies.lax,
    ": ignore most recoverable errors and keep going";
    "-L", taskc Task.lib,
    "<dir>: use all cmi files in <dir> in the analysis";
    "-no-alias-deps", set_t transparent_aliases, ": delay aliases dependencies";
    "-nested", set_t nested,
    ": Map file path <Subdir/ … /file> to module path <Subdir. … .File>, \
     instead of just <File>";
    "-o", Cmd.String ( (:=) output ), "<filename>: mode current output file";
    "-ancestors-of", task_p Task.add_seed,
    "<filename>: only analyze files which are an ancestor of <filename>";
    "-extension-node", Cmd.Bool (use_p transparent_extension_nodes),
    "<bool>: inspect unknown extension nodes\n"
    ^ "\n\n Codept only modes:\n";

    "-deps", Unit (mode @@ Modes.Deps None),
    ": print dependencies using the currently selected structured format\
     (default:json)";
    "-info", Unit (mode Modes.Info), ": print detailed information";
    "-json", Unit (mode @@ Modes.Deps (Some Schematic.Json)),
    ": print dependencies in json format";
    "-json-schema", String print_json_schema,
    "<mode>: print json schema for the corresponding mode (deps,sig,m2l)";
    "-sexp", Unit (mode @@ Modes.Deps (Some Schematic.Sexp)),
    ": print dependencies in a s-expression format";

    "-export", String (fun s -> mode (Modes.Export s) ()),
    "<name>: export resolved modules signature";
    "-dot", Unit (mode Modes.Dot), ": print dependencies in dot format";
    "-makefile", Unit (makefile_c action), ": print makefile depend file(default)";
    "-approx-m2l", Unit (set_iter Single.Approx_file),
    ": print approximated m2l ast";
    "-m2l-info", Unit (set_iter Single.M2l_info), ": print m2l ast";
    "-m2l", Unit (set_iter Single.M2l),
    ": print m2l ast in structured format";
    "-one-pass", Unit (set_iter Single.One_pass), ": print m2l ast after one pass";
    "-sig", Unit (mode Modes.Signature), ": print inferred signature";
    "-sig-only", set_t sig_only,
    ": filter produced m2l to keep only signature-level elements.\
     \n\n Module suboptions:\n";

    "-nl-modules", Unit (mode @@ Modes.(Modules(Nl,Dep)) ),
    ": print new-line separated raw dependencies";
    "-extern-modules", Unit (mode @@ Modes.(Modules(Standard,Lib))),
    ": print raw extern dependencies";
    "-inner-modules", Unit (mode @@ Modes.(Modules(Standard,Inner)) ),
    ": print raw inner dependencies";
    "-unknown-modules", Unit (mode @@ Modes.(Modules(Standard,Extern)) ),
    ": print raw unresolved dependencies\n\n Findlib options: \n";

    "-pkg", pkg param fquery,
    "<pkg_name>: use the ocamlfind package <pkg_name> during the analysis";
    "-package", pkg param fquery, "<pkg_name>: same as pkg";
    "-predicates", findlib Findlib.predicates,
    "<p1,...,p_n>: add predicates to ocamlfind processing";
    "-ppxopt", findlib Findlib.ppxopt,
    "<ppx,opt>: add <opt> as an option of <ppx>";
    "-ppopt", findlib Findlib.ppopt,
    "<ppopt>: add <opt> to the active pp preprocessor";
    "-syntax", findlib Findlib.syntax,
    "<syntax_name>: use the <syntax> preprocessor provided \
     by one of the available findlib packages.";
    "-native-filter", set_t native,
    ": generate native compilation only dependencies";
    "-bytecode-filter", set_t bytecode,
    ": generate bytecode only dependencies.\n\n Fault options:\n";

    "-closed-world", set_t closed_world,
    ": require that all dependencies are provided";
    "-strict", set_p policy Codept_policies.strict,
    ": fail rather than approximate anything";
    "-quiet", set_p policy Codept_policies.quiet,
    ": ignore and silent all recoverable errors and keep going";

    "-fatal", String (exit_level param),
    "<level>: exit for fault at level <level> and beyond.";
    "-fault", String (fault param),
    "<fault.path=level>: update fault policy for the given fault.";
    "-fault-doc", Unit (print_policy param), ": show fault policy documentation";
    "-verbosity", String (silent_level param),
    "<level>: only print fault beyond level <level>, \
     with level in {info,notification,warning,error,critical}"
    ^ "\n\n Misc options:\n";

    "-no-implicits", set_f implicits,
    ": do not implicitly search for a mli \
     file when given a ml file input";
    "-no-include", set_t no_include, ": do not include base directory by default";
    "-no-stdlib", no_stdlib param, ": do not use precomputed stdlib environment";
    "-read-sig", taskc Task.add_sig,
    "<signature>: add signature to the base environment";
    "-see", task_p Task.add_invisible_file,
    "<file>: use <file> in dependencies computation but do not display it."

    ^ "\n\nGeneric IO settings:\n";

    "-format", String (format param),
    "<format>: use <format> for input and output files\n\nMisc generic options:";
  ]


type query =
  {
    action:action;
    findlib:Findlib.query;
    params: Params.t;
    task:Common.task
  }


let stderr= Format.err_formatter


let process version ?(extra=[]) argv =
  let params = ref param0
  and task = ref task0
  and findlib_query = ref findlib_query0
  and action = ref action0 in
  let args = Arg.align @@ extra @ args action params task findlib_query version in
    Compenv.readenv stderr Before_args;
    begin
      try Extended_args.parse  argv args (Task.add_file params task) usage_msg
      with
      | Arg.Bad msg | Arg.Help msg ->
        (print_endline msg; exit 2)
    end
  ; if not !params.no_include then add_include params "."
  ; if !action = action0 then
    makefile_c action ()
  ; { params= !params; task = !task; findlib = !findlib_query; action = !action }

let translate_findlib_query task query =
  let task = ref task in
  let result = Findlib.process query in
  Pp.fp Pp.std "pp? %a\n" Pp.(opt string) result.pp;
  Option.iter ( fun p -> Clflags.preprocessor := Some p ) result.pp;
  List.iter (Task.lib task) result.libs; List.iter add_ppx result.ppxs;
  !task
