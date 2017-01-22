open Params
module Cmd = Arg
module Pkg = Paths.Pkg
module Pth = Paths.Simple

type action = {
  active_modes: (Unit.r list Unit.pair -> string * (Format.formatter -> unit))  list;
  action: (string * (Format.formatter -> unit) ) list
}

let action0 = { active_modes = []; action = [] }

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

let param0 = {

    makefile = {
      all = false;
      native = false;
      bytecode = false;
      abs_path = false;
      slash = Filename.dir_sep;
      implicits = true;
    };

    common= {
      includes = Name.Map.empty;
      synonyms;
    };

    analyzer = {
      transparent_aliases = false;
      transparent_extension_nodes = true;
      precomputed_libs = Name.Set.singleton "stdlib";
      closed_world = false;
      sig_only = false;
      polycy = Codept_polycy.polycy;
    };

    no_include = false;
    may_approx = false;
    output = "%"
  }

let task0 : Common.task = {
    Common.files = [];
    seeds = [];
    invisibles = Paths.S.Set.empty;
    libs = [];
    opens = [];
  }

let findlib_query0 =  Findlib.empty

let makefile_c ppf param task =
  Makefile.main ppf param.common param.makefile task

(** {2 Option implementations } *)
let first_ppx = Compenv.first_ppx

let add_ppx ppx =
  first_ppx := ppx :: !first_ppx

let ml_synonym param s =
  let open L in
  let synonyms = !param.[synonyms] in
  let info = { Common.format = Src; kind = Implementation } in
  let synonyms =   Name.Map.add s info synonyms in
  param.[L.synonyms] <- synonyms

let mli_synonym param s =
  let open L in
  let synonyms = !param.[synonyms] in
  let info = { Common.format = Src; kind = Interface } in
  let synonyms =   Name.Map.add s info synonyms in
  param.[L.synonyms] <- synonyms


let act (file,action) =
  if file = "%" then
    action Pp.std
  else
    let f= open_out file in
    let f'= Format.formatter_of_out_channel f in
    action f';
    Format.pp_print_flush f' ();
    close_out f


let mode  action param command () =
  let output = !param.output in
  action  :=
    { !action with
      active_modes =
        (fun units -> (output, fun out -> command out !param units) )
        :: !action.active_modes
    }

let set_iter action param (task:Common.task ref) command () =
  action :=
    { !action with
      action =
        ( !param.output,
          begin
            fun out ->
              List.iter (command out !param) (!task).files
          end
        ) :: (!action).action
    }

let print_vnum version ()= Format.printf "%.2f@." version
let print_version version ()= Format.printf "codept, version %.2f@." version

let add_include param dir =
  let files = Sys.readdir dir in
  let dir = if dir = "." then [] else Paths.S.parse_filename dir in
  let open L in
  let includes =
    Array.fold_left (fun m x ->
        let polycy =
          let open Fault in
          Polycy.set_err (Codept_polycy.unknown_extension, Level.whisper)
            !param.[polycy] in
        match Task.classify polycy L.( !param.[synonyms] ) x with
        | None | Some { kind = Signature; _ } -> m
        | Some { kind = Interface | Implementation ; _ } ->
          Name.Map.add (Read.name x)
            Pkg.( dir / local x) m
      )
      !param.[includes] files
  in
  param.[L.includes] <- includes

let fault param s =
  match String.split_on_char '=' s with
  | [] | [_]| _ :: _ :: _ :: _ -> ()
  | [a;b] ->
    let path= List.map String.trim @@ String.split_on_char '.' a in
    let level = Fault.Level.of_string b in
    let open L in
    let polycy = !param.[polycy] in
    param.[L.polycy] <- Fault.Polycy.set (path,None,level) polycy

let silent_level param s =
  let open L in
  let polycy = !param.[polycy] in
  param.[L.polycy] <- { polycy with silent = Fault.Level.of_string s}

let exit_level param s =
  let open L in
  let polycy = !param.[polycy] in
  param.[L.polycy] <- { polycy with exit = Fault.Level.of_string s}

let print_polycy param ()=
  Fault.Polycy.pp Pp.std L.(!param.[polycy])

module Filter = struct
  let inner = function
    | { Pkg.source = Local; _ } -> true
    |  _ -> false

  let dep = function
    | { Pkg.source = (Unknown|Local); _ } -> true
    |  _ -> false


  let extern = function
    | { Pkg.source = Unknown; _ } -> true
    | _ -> false

  let lib = function
    | { Pkg.source = (Pkg _ | Special _ ) ; _ } -> true
    | _ -> false
end

let set_p param lens value =
  let open L in
  Cmd.Unit( fun () -> param.[lens] <- value)

let set_t param lens = set_p param lens true
let set_f param lens = set_p param lens false


let use_p param lens value =
  let open L in
  param.[lens] <- value

let task_p param task f = Cmd.String (f param task)
let taskc task f = Cmd.String (f task)

let findlib findlib_query update =
  Cmd.String (fun s -> findlib_query := update !findlib_query s)

let pkg param findlib_query =
  Cmd.String( fun s ->
      if Common.is_stdlib_pkg s then
        let open L in
        param.[precomputed_libs] <- Name.Set.add s !param.[precomputed_libs]
      else
        findlib_query := Findlib.pkg !findlib_query s;
    )

let no_stdlib param =
  Cmd.Unit( fun () ->
      let open L in
      param.[precomputed_libs] <-
        Name.Set.remove "stdlib" !param.[precomputed_libs]
    )

let usage_msg =
  "Codept is an alternative dependency solver for OCaml.\n\
   Usage: codept [options] [⟨signature files⟩] [⟨source files⟩] [⟨m2l files⟩]\n\
   − ⟨m2l⟩ files are serialized m2l ast files, identified by either a ⟨.m2l⟩ or\
     ⟨.m2li⟩ extension.\n\
   − ⟨signature⟩ files are signature information files, identified by \
   a ⟨.sig⟩ extension.\n\
   These two files format are useful to save persistent information between \
   multiple calls to codept.\n\n\
   Non-existent files and files with an unknown extension are ignored.\
   \n\
   The following options are common with ocamldep:\n"
open L
open Task
let args action param task fquery version =
  let set_t = set_t param in
  let set_f = set_f param in
  let use_p x =use_p param x in
  let set_p x = set_p param x in
  let task_p = task_p param task in
  let taskc = taskc task in
  let mode = mode action param in
  let set_iter = set_iter action param task in
  let findlib = findlib fquery in

  Cmd.[
    "-absname", set_t abs_path, ": use absolute path name";
    "-all", set_t all, ": display full dependencies in makefile";
    "-allow-approx", set_p polycy Fault.Polycy.parsing_approx,
    ": fall back to approximated parser \
                                        in presence of syntax errors.";
    "-as-map", task_p as_map, "<file>: same as \
                                   \"-no-alias-deps <file>\"";
    "-I", String (add_include param),
    "<dir>: do not filter files in <dir> when printing dependencies";
    "-impl", taskc (add_impl Src), "<f>: read <f> as a ml file";
    "-intf", taskc (add_intf Src), "<f>: read <f> as a mli file";
    "-map", task_p map, "<file>: same as \"-no-alias-deps \
                            -see <file>\"";
    "-ml-synonym", String (ml_synonym param),
    "<s>: use <s> extension as a synonym for ml";
    "-mli-synonym", String (mli_synonym param),
    "<s>: use <s> extension as a synonym for mli";
    "-modules", Unit (mode @@ Modes.modules ~filter:Filter.dep),
    ": print raw module dependencies";
    "-native", set_t native, ": generate native compilation only dependencies";
    "-bytecode", set_t bytecode, ": generate bytecode only dependencies";

    "-one-line", Cmd.Unit ignore, ": does nothing";
    "-open", taskc add_open, "<name>: open module <name> at the start of \
                             all compilation units \n\
                             (except units whose name is <name>).";
    "-pp", Cmd.String(fun s -> Clflags.preprocessor := Some s),
    "<cmd>: pipe sources through preprocessor <cmd>";
    "-ppx", Cmd.String add_ppx,
    "<cmd>: pipe abstract syntax trees through ppx preprocessor <cmd>";
    "-slash", set_p slash "/", ": use forward slash as directory separator";
    "-sort", Unit(mode Modes.sort),": sort files according to their dependencies";
    "-version", Cmd.Unit (print_version version),
    ": print human-friendly version description";
    "-vnum", Cmd.Unit (print_vnum version), ": print version number\
                                             \n\n Codept only modes:\n";


    "-aliases", Unit (mode Modes.aliases), ": print aliases";
    "-info", Unit (mode Modes.info), ": print detailed information";
    "-export", Unit (mode Modes.export), ": export resolved modules signature";

    "-dot", Unit (mode Modes.dot), ": print dependencies in dot format";
    "-makefile", Unit (mode makefile_c), ": print makefile depend file(default)";
    "-approx-m2l", Unit (set_iter Single.approx_file),
    ": print approximated m2l ast";
    "-m2l", Unit (set_iter Single.m2l), ": print m2l ast";
    "-m2l-sexp", Unit (set_iter Single.m2l_sexp),
    ": print m2l ast in s-expression format";
    "-one-pass", Unit (set_iter Single.one_pass), ": print m2l ast after one pass";
    "-sig", Unit (mode Modes.signature), ": print inferred signature";
    "-sig-only", set_t sig_only,
    ": filter produced m2l to keep only signature-level elements.\
     \n\n Module suboptions:\n";

    "-nl-modules", Unit (mode @@ Modes.line_modules ~filter:Filter.dep),
    ": print new-line separated raw dependencies";
    "-extern-modules", Unit (mode @@ Modes.modules ~filter:Filter.lib),
    ": print raw extern dependencies";
    "-inner-modules", Unit (mode @@ Modes.modules ~filter:Filter.inner),
    ": print raw inner dependencies";
    "-unknown-modules", Unit (mode @@ Modes.modules ~filter:Filter.extern),
    ": print raw unresolved dependencies\n\n Findlib options: \n";

    "-pkg", pkg param fquery,
    "<pkg_name>: use the ocamlfind package <pkg_name> during the analysis";
    "-package", pkg param fquery, "<pkg_name>: same as pkg";
    "-predicates", findlib Findlib.predicates,
    "<comma-separated list of string>: add predicates to ocamlfind processing";
    "-ppxopt", findlib Findlib.ppxopt,
    "<ppx,opt>: add <opt> as an option of <ppx>";
    "-ppopt", findlib Findlib.ppopt,
    "<ppopt>: add <opt> to the active pp preprocessor";
    "-syntax", findlib Findlib.syntax,
    "<syntax name>: use the <syntax> preprocessor provided \
     by one of the available findlib packages.";
    "-native-filter", set_t native,
    ": generate native compilation only dependencies";
    "-bytecode-filter", set_t bytecode,
    ": generate bytecode only dependencies.\n\n Fault options:\n";

    "-closed-world", set_t closed_world,
    ": require that all dependencies are provided";
    "-k", set_p polycy Codept_polycy.lax,
    ": ignore most recoverable errors and keep going";
    "-strict", set_p polycy Codept_polycy.strict,
    ": fail rather than approximate anything";
    "-quiet", set_p polycy Codept_polycy.quiet,
    ": ignore and silent all recoverable errors and keep going";
    "-fault", String (fault param),
    "<fault.path=level>: update fault polycy for the given fault.";
    "-fault-doc", Unit (print_polycy param), ": show fault polycy documentation";
    "-silent-fault-level", String (silent_level param),
    "<level>: only print fault beyond level <level>";
    "-exit-fault-level", String (exit_level param),
    "<level>: exit for fault at level <level> and beyond.\n\n Misc options:\n";

    "-only-ancestors-of", task_p Task.add_seed,
    "<module name>: only analyze files which are an ancestor of <module name>";
    "-L", taskc lib, "<dir>: use all cmi files in <dir> \
                       in the analysis";
    "-no-alias-deps", set_t transparent_aliases, ": delay aliases dependencies";
    "-o", Cmd.String (use_p output), "<filename>: mode current output file";
    "-no-implicits", set_f implicits,
    ": do not implicitly search for a mli \
     file when given a ml file input";
    "-no-include", set_t no_include, ": do not include base directory by default";
    "-no-stdlib", no_stdlib param, ": do not use precomputed stdlib environment";
    "-read-sig", taskc add_sig,
    "<signature>: add signature to the base environment";
    "-see", task_p add_invisible_file,
    "<file>: use <file> in dependencies computation but do not display it.";
    "-transparent-extension-node", Cmd.Bool (use_p transparent_extension_nodes),
    "<bool>: inspect unknown extension nodes\n"
  ]


type query =
  {
    action:action;
    findlib:Findlib.query;
    params: Params.t;
    task:Common.task
  }


let stderr= Format.err_formatter


let process version argv =
  let params = ref param0
  and task = ref task0
  and findlib_query = ref findlib_query0
  and action = ref action0 in
  let args = args  action params task findlib_query version in
    Compenv.readenv stderr Before_args
  ; Cmd.parse_argv argv args (add_file params task) usage_msg
  ; if not !params.no_include then add_include params "."
  ; Compenv.readenv stderr Before_link
  ; if !action = action0 then
    mode action params makefile_c ()
  ; { params= !params; task = !task; findlib = !findlib_query; action = !action }

let translate_findlib_query task query =
  let task = ref task in
  let result = Findlib.process query in
  Clflags.preprocessor := result.pp;
  List.iter (lib task) result.libs; List.iter add_ppx result.ppxs;
  !task
