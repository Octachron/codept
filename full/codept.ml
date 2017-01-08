module Cmd = Arg
module U = Unit
module Pkg = Paths.Pkg
module Pth = Paths.Simple

(** Utility functions and module *)
open Params
let tool_name = "codept"
let version = 0.3
let stderr= Format.err_formatter
let std = Format.std_formatter

let synonyms =
  let open Resource in
  let add = Name.Map.add in
  Name.Map.empty
  |> add "ml" {kind=Implementation; format = Src}
  |> add "mli" {kind=Interface; format = Src}
  |> add "cmi" {kind=Interface; format = Cmi}
  |> add "m2l" {kind=Implementation; format = M2l}
  |> add "m2li" {kind=Interface; format = M2l}
  |> add "sig" {kind = Signature; format = Src }

let param = ref {

    makefile = {
      all = false;
      native = false;
      bytecode = false;
      abs_path = false;
      sort = false;
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
      no_stdlib = false; std_otherlibs = false;
      closed_world = false;
      sig_only = false;
      polycy = Codept_polycy.polycy;
    };

    no_include = false;
    may_approx = false;
    output = "%"
  }

let task: Common.task ref = ref {
    Common.files = { Unit.ml = []; Unit.mli = [] };
    signatures = [];
    invisibles = Pth.Set.empty;
    libs = [];
    opens = [];
  }

let makefile_c ppf param task =
  Makefile.main ppf param.common param.makefile task

(** {2 Option implementations } *)
let first_ppx = Compenv.first_ppx

let add_ppx ppx =
  first_ppx := ppx :: !first_ppx

let ml_synonym s =
  let open L in
  let synonyms = !param.[synonyms] in
  let info = { Resource.format = Src; kind = Implementation } in
  let synonyms =   Name.Map.add s info synonyms in
  param.[L.synonyms] <- synonyms

let mli_synonym s =
  let open L in
  let synonyms = !param.[synonyms] in
  let info = { Resource.format = Src; kind = Interface } in
  let synonyms =   Name.Map.add s info synonyms in
  param.[L.synonyms] <- synonyms

let action = ref []
let active_modes = ref []

let act (file,action) =
  if file = "%" then
    action Pp.std
  else
    let f= open_out file in
    let f'= Format.formatter_of_out_channel f in
    action f';
    Format.pp_print_flush f' ();
    close_out f


let mode command () =
  let output = !param.output in
  active_modes :=
    (fun units -> (output, fun out -> command out !param units) )
                 :: !active_modes

let set_iter command () = action :=
    ( !param.output,
      begin
        fun out ->
          let {Unit.ml;mli} = (!task).files  in
          List.iter (command out !param) (ml @ mli)
      end
    ) :: !action

let print_vnum ()= Format.printf "%.2f@." version
let print_version ()= Format.printf "codept, version %.2f@." version

let add_include dir =
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

let fault s =
  match String.split_on_char '=' s with
  | [] | [_]| _ :: _ :: _ :: _ -> ()
  | [a;b] ->
    let path= List.map String.trim @@ String.split_on_char '.' a in
    let level = Fault.Level.of_string b in
    let open L in
    let polycy = !param.[polycy] in
    param.[L.polycy] <- Fault.Polycy.set (path,None,level) polycy

let silent_level s =
  let open L in
  let polycy = !param.[polycy] in
  param.[L.polycy] <- { polycy with silent = Fault.Level.of_string s}

let exit_level s =
  let open L in
  let polycy = !param.[polycy] in
  param.[L.polycy] <- { polycy with exit = Fault.Level.of_string s}

let print_polycy ()=
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

let set_p lens value =
  let open L in
  Cmd.Unit( fun () -> param.[lens] <- value)

let set_t lens = set_p lens true
let set_f lens = set_p lens false


let use_p lens value =
  let open L in
  param.[lens] <- value

let task_p f = Cmd.String (f param task)
let taskc f = Cmd.String (f task)

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
let args = Cmd.[
    "-absname", set_t abs_path, ": use absolute path name";
    "-all", set_t all, ": display full dependencies in makefile";
    "-allow-approx", set_p polycy Fault.Polycy.parsing_approx,
    ": fall back to approximated parser \
                                        in presence of syntax errors.";
    "-as-map", task_p as_map, "<file>: same as \
                                   \"-no-alias-deps <file>\"";
    "-I", String add_include,"<dir>: do not filter files in <dir> when printing \
                          dependencies";
    "-impl", taskc (add_impl Src), "<f>: read <f> as a ml file";
    "-intf", taskc (add_intf Src), "<f>: read <f> as a mli file";
    "-map", task_p map, "<file>: same as \"-no-alias-deps \
                            -see <file>\"";
    "-ml-synonym", String ml_synonym, "<s>: use <s> extension as a synonym \
                                       for ml";
    "-mli-synonym", String ml_synonym, "<s>: use <s> extension as a synonym \
                                        for mli";
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
    "-sort", set_t sort, ": sort dependencies when printing";
    "-version", Cmd.Unit print_version,
    ": print human-friendly version description";
    "-vnum", Cmd.Unit print_vnum, ": print version number\n\n Codept only modes:\n";


    "-aliases", Unit (mode Modes.aliases), ": print aliases";
    "-info", Unit (mode Modes.info), ": print detailed information";
    "-export", Unit (mode Modes.export), ": export resolved modules signature";

    "-dot", Unit (mode Modes.dot), ": print dependencies in dot format";
    "-dsort", Unit(mode Modes.dsort),": print unit paths in topological order";
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

    "-pkg", Cmd.String Findlib.(update pkg),
    "<pkg_name>: use the ocamlfind package <pkg_name> during the analysis";
    "-package", Cmd.String Findlib.(update pkg), "<pkg_name>: same as pkg";
    "-predicates", Cmd.String Findlib.(update predicates),
    "<comma-separated list of string>: add predicates to ocamlfind processing";
    "-ppxopt", Cmd.String Findlib.(update ppxopt),
    "<ppx,opt>: add <opt> as an option of <ppx>";
    "-ppopt", Cmd.String Findlib.(update ppopt),
    "<ppopt>: add <opt> to the active pp preprocessor";
    "-syntax", Cmd.String Findlib.(update syntax),
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
    "-fault", String fault, "<fault.path=level>: update fault polycy for the given\
                             fault.";
    "-fault-doc", Unit print_polycy, ": show fault polycy documentation";
    "-silent-fault-level", String silent_level,
    "<level>: only print fault beyond level <level>";
    "-exit-fault-level", String exit_level,
    "<level>: exit for fault at level <level> and beyond.\n\n Misc options:\n";

    "-L", taskc lib, "<dir>: use all cmi files in <dir> \
                       in the analysis";
    "-no-alias-deps", set_t transparent_aliases, ": delay aliases dependencies";
    "-o", Cmd.String (use_p output), "<filename>: mode current output file";
    "-no-implicits", set_f implicits,
    ": do not implicitly search for a mli \
     file when given a ml file input";
    "-no-include", set_t no_include, ": do not include base directory by default";
    "-no-stdlib", set_t no_stdlib, ": do not use precomputed stdlib environment";
    "-std-otherlibs", set_t std_otherlibs,
    ": use precomputed signature for stdlib otherlibs, \
     i.e. bigarray, threads … ";
    "-read-sig", taskc read_sig,
    "<signature>: add signature to the base environment";
    "-see", task_p add_invisible_file,
    "<file>: use <file> in dependencies computation but do not display it.";
    "-transparent-extension-node", Cmd.Bool (use_p transparent_extension_nodes),
    "<bool>: inspect unknown extension nodes\n"
  ]

let () =
  Compenv.readenv stderr Before_args
  ; if not !param.no_include then add_include "."
  ; Cmd.parse args (add_file param task) usage_msg
  ; let libs, ppxs = Findlib.process () in
    List.iter (lib task) libs; List.iter (Option.iter add_ppx) ppxs
  ; Compenv.readenv stderr Before_link
  ; if !action = [] && !active_modes = [] then
    mode makefile_c ()
  ; List.iter act !action
  ; if not (!active_modes = []) then
    let analyzed = Analysis.main !param.analyzer !task in
    List.iter (fun f -> act @@ f analyzed) !active_modes
