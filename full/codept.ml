module Cmd = Arg
module U = Unit
module Pkg = Paths.Pkg
module Pth = Paths.Simple

(** Utility functions and module *)
open M2l
module S = Module.Sig
let (%) f g x = f (g x)

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



exception Unknown_file_type of string

let extension name =
  let n = String.length name in
  let r = try String.rindex name '.' with Not_found -> n-1 in
  String.sub name (r+1) (n-r-1)

let classify polycy synonyms f =
  let ext = extension f in
  match Name.Map.find ext synonyms with
  | x -> Some x
  | exception Not_found ->
    Fault.handle polycy Codept_polycy.unknown_extension ext; None

(** Printing directly from source file *)
let to_m2l polycy sig_only (k,f) =
    match Read.file k f with
    | _name, Ok x ->
      if sig_only then Some (M2l.Sig_only.filter x) else Some x
    | _, Error (Ocaml msg) -> Fault.handle polycy Fault.syntaxerr msg; None
    | _, Error M2l -> Fault.handle polycy Codept_polycy.m2l_syntaxerr f; None


let approx_file ppf _param (_,f) =
  let _name, lower, upper = Approx_parser.file f in
  Pp.fp ppf  "lower bound:%a@. upper bound:%a@."
    M2l.pp lower M2l.pp upper

let one_pass ppf param ( (_,filename) as f) =
  let param = param.analyzer in
  let module Param = (val Analysis.lift param) in
  let module Sg = Interpreter.Make(Envts.Base)(Param) in
  let start = to_m2l param.polycy param.sig_only f in
  match Option.( start >>| Sg.m2l (Pkg.local filename) Envts.Base.empty ) with
  | None -> ()
  | Some (Ok (_state,d)) -> Pp.fp ppf "Computation finished:\n %a@." S.pp d
  | Some (Error h) -> Pp.fp ppf "Computation halted at:\n %a@." M2l.pp h

let m2l ppf param f =
  let param = param.analyzer in
  let start = to_m2l param.polycy param.sig_only f in
  let open Option in
  start
  >>| Normalize.all
  >>| snd
  >>| Pp.fp ppf  "%a@." M2l.pp
  >< ()

let m2l_sexp ppf param f =
  let param = param.analyzer in
  let start = to_m2l param.polycy param.sig_only f in
  let open Option in
  start
  >>| Normalize.all
  >>| snd
  >>| M2l.sexp.embed
  >>| Pp.fp ppf  "%a@." Sexp.pp
  >< ()


let analyze param = Analysis.main param.analyzer

let info ppf param task =
  let {Unit.ml; mli} = analyze param task in
  let print =  Pp.(list ~sep:(s" @,") @@ Unit.pp ) ppf in
  print ml; print mli

let export ppf param task =
  let {Unit.mli; _} = analyze param task in
  let sign (u:Unit.r)= u.signature in
  let md (unit:Unit.r) =
    Module.M {Module.
      name = unit.name
    ; origin = Unit { source=Pkg.Special "exported"; file = [unit.name] }
    ; args = []
    ; signature = sign unit
    ;  precision = Exact
    } in
  let s =
    let open Module.Sig in
    List.fold_left (fun sg u -> merge sg @@ create @@ md u) empty mli
  in
  Pp.fp ppf "@[<hov>let signature=@;\
             let open Module in @;\
             let open Sig in @;\
             %a\
             @]@." Module.reflect_signature s

let sign ppf param task =
  let {Unit.mli; _} = analyze param task in
  let md {Unit.signature; name; path; _  } =
    Module.M ( Module.create ~args:[]
      ~origin:(Unit path)
      name signature
             )
  in
  let mds = List.map md mli in
  let sexp = Sexp.( (list Module.sexp).embed ) mds in
  Pp.fp ppf "@[%a@]@." Sexp.pp sexp

let dependencies ?filter sort (u:Unit.r) =
  Pkg.Set.elements u.dependencies
  |> sort
  |> (match filter with
      | Some f -> List.filter f
      | None -> fun x -> x
    )
  |> List.map Pkg.module_name


let aliases (x:Unit.r) = Module.(aliases @@ M (create "" x.signature) )

let pp_module {Makefile.abs_path;slash; _ } proj ppf (u:Unit.r) =
  let pp_pkg = Pkg.pp_gen slash in
  let elts = proj u in
  Pp.fp ppf "%a: %a\n" pp_pkg (Makefile.make_abs abs_path u.path)
    Pp.( list ~sep:(s" ") Name.pp )
    elts


let pp_aliases ppf param (task:Common.task) =
  let param = param.makefile in
  let pp_pkg = Pkg.pp_gen param.slash in
  let pp_m m =
    let open Module in
    match m with
    | M { origin = Unit path; _ } as m ->
      let path' = Pkg.update_extension
          (function "m2l" -> ".ml" | "m2li" -> ".mli" | s -> s ) path in
      let f = Makefile.make_abs param.abs_path path' in
      Pp.fp ppf "%a: %a\n" pp_pkg f
        Pp.( list ~sep:(s" ") Name.pp ) (aliases m)
    | _ -> () in
      List.iter pp_m task.signatures


let inner_filter = function
  | { Pkg.source = Local; _ } -> true
  |  _ -> false

let dep_filter = function
  | { Pkg.source = (Unknown|Local); _ } -> true
  |  _ -> false


let extern_filter = function
  | { Pkg.source = Unknown; _ } -> true
  | _ -> false

let lib_filter = function
  | { Pkg.source = (Pkg _ | Special _ ) ; _ } -> true
  | _ -> false

let id x = x
let upath x = x.Unit.path

let sort proj param mli =
  let order = Sorting.order mli in
  if param.makefile.sort then Sorting.toposort order proj
  else id

let gen_modules proj ppf param task =
  let {Unit.ml; mli} = analyze param task in
  let sort_p = sort id param mli in
  let sort_u = sort upath param mli in
  let print units = Pp.fp ppf "%a"
      Pp.(list ~sep:(s"") @@ pp_module param.makefile @@ proj sort_p)
      (sort_u units) in
  print ml; print mli

let modules ?filter =
  gen_modules (dependencies ?filter)


let pp_only_deps sort ?filter ppf u =
  let open Unit in
  let elts = Pkg.Set.elements u.dependencies in
  let elts = sort elts in
  let elts = match filter with
    | Some f -> List.filter f elts
    | None -> elts in
  Pp.fp ppf "%a"
    Pp.( list ~sep:(s"\n") Name.pp )
    ( List.map Pkg.module_name elts)

let line_modules ?filter ppf param task =
  let {Unit.ml; mli} = analyze param task in
  let sort_p = sort id param mli in
  let sort_u = sort upath param mli in
  let print units = Pp.fp ppf "%a"
      Pp.(list ~sep:(s"") @@ pp_only_deps sort_p ?filter)
      (sort_u units) in
  print ml; print mli

let local_dependencies sort unit =
  sort
  @@ List.filter
    (function {Pkg.source=Unknown; _ }
            | {Pkg.source=Special _ ; _ } -> false | _ -> true )
  @@ Pkg.Set.elements unit.U.dependencies


let dot ppf param task =
  let open Unit in
  let {mli; _ } = analyze param task in
  let sort = sort id param mli in
  Pp.fp ppf "digraph G {\n";
  List.iter (fun u ->
      List.iter (fun p ->
          Pp.fp ppf "%s -> %s \n" u.name @@ Pkg.module_name p)
        (local_dependencies sort u)
    ) mli;
  Pp.fp ppf "}\n"

let local_deps x =
  let filter = function { Pkg.source = Local; _ } -> true | _ -> false in
  x.Unit.dependencies |> Pkg.Set.filter filter
  |> Pkg.Set.elements
  |> List.map (Pkg.change_extension ".ml")
  |> Pkg.Set.of_list


let dsort ppf param task =
  let units: _ Unit.pair = analyze param task in
  let gs = Unit.Groups.R.group units in
  let extract_path _ g l = match g with
    | { Unit.ml = Some x; mli = _ }
    | { ml = None; mli = Some x }  -> x.Unit.path :: l
    | { ml = None; mli = None } -> l in
  let paths =
    Paths.S.Map.fold extract_path gs []  in
  let deps path =
    let key = path.Pkg.file in
    match Unit.Groups.R.Map.find key gs with
    | { ml = Some x; mli = Some y } ->
      Pkg.Set.union (local_deps x) (local_deps y)
    | { ml = Some x; mli = None } | { mli= Some x; ml =None } -> local_deps x
    | { ml = None; mli = None } -> Pkg.Set.empty in
  Sorting.full_topological_sort deps paths
  |> Pp.list ~sep:Pp.(s" ") ~post:Pp.(s"\n") Pkg.pp ppf

let task: Common.task ref = ref {
    Common.files = { Unit.ml = []; Unit.mli = [] };
    signatures = [];
    invisibles = Pth.Set.empty;
    libs = [];
    opens = [];
  }

let makefile_c ppf param task =
  Makefile.main ppf param.common param.analyzer param.makefile task

(** {2 Option implementations } *)
let add_invi name =
  task := { !task with
            invisibles = Pth.Set.add (Paths.S.parse_filename name) (!task).invisibles
          }

let add_impl format name =
  let k = { Read.kind = Structure; format } in
  let {Unit.ml;mli} = (!task).files in
  task := { !task with files = { ml = (k,name) :: ml; mli } }

let add_intf format name =
  let k = { Read.kind = Signature; format } in
  let {Unit.ml;mli} = !(task).files in
  task := {!task with files = { mli = (k,name) :: mli; ml } }


let parse_sig lexbuf=
  Sexp.( (list Module.sexp).parse )
  @@ Sexp_parse.many Sexp_lex.main
  @@ lexbuf

let read_sigfile filename =
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
  let sigs = parse_sig lexbuf in
  close_in chan;
  sigs

let add_sig more =
  let sigs = !(task).signatures in
  Option.iter (fun more ->
      task := {!task with signatures = more @ sigs  })
    more

let read_sig ssig =
  add_sig
  @@ parse_sig
  @@ Lexing.from_string ssig

let add_file name =
  if Sys.file_exists name then
    match classify !param.analyzer.polycy !param.common.synonyms name with
    | None -> ()
    | Some { kind = Implementation; format } ->
      add_impl format name
    | Some { kind = Interface; format } -> add_intf format name
    | Some { kind = Signature; _ } -> add_sig @@ read_sigfile name

let add_invisible_file name =
  if Sys.file_exists name then
    ( add_invi name;
      add_file name
    )

let add_open name =
  task := { !task with opens = [name] :: (!task).opens }

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

let lib f =
  task := { !task with libs = f :: (!task).libs }

let action = ref []

let act (file,action) =
  if file = "%" then
    action Pp.std
  else
    let f= open_out file in
    let f'= Format.formatter_of_out_channel f in
    action f';
    Format.pp_print_flush f' ();
    close_out f


let setc command () =
  action:= (!param.output, fun out -> command out !param !task) :: !action

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


let map file =
  L.( param.[transparent_aliases] <- true );
  add_invisible_file file

let as_map file =
  L.( param.[transparent_aliases] <- true ) ;
  add_file file

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
        match classify polycy L.( !param.[synonyms] ) x with
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

let set_p lens value =
  let open L in
  Cmd.Unit( fun () -> param.[lens] <- value)

let set_t lens = set_p lens true
let set_f lens = set_p lens false


let use_p lens value =
  let open L in
  param.[lens] <- value


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
let args = Cmd.[
    "-absname", set_t abs_path, ": use absolute path name";
    "-all", set_t all, ": display full dependencies in makefile";
    "-allow-approx", set_p polycy Fault.Polycy.parsing_approx,
    ": fall back to approximated parser \
                                        in presence of syntax errors.";
    "-as-map", Cmd.String as_map, "<file>: same as \
                                   \"-no-alias-deps <file>\"";
    "-I", String add_include,"<dir>: do not filter files in <dir> when printing \
                          dependencies";
    "-impl", String (add_impl Src), "<f>: read <f> as a ml file";
    "-intf", String (add_intf Src), "<f>: read <f> as a mli file";
    "-map", Cmd.String map, "<file>: same as \"-no-alias-deps \
                            -see <file>\"";
    "-ml-synonym", String ml_synonym, "<s>: use <s> extension as a synonym \
                                       for ml";
    "-mli-synonym", String ml_synonym, "<s>: use <s> extension as a synonym \
                                        for mli";
    "-modules", Unit (setc @@ modules ~filter:dep_filter),
    ": print raw module dependencies";
    "-native", set_t native, ": generate native compilation only dependencies";
    "-bytecode", set_t bytecode, ": generate bytecode only dependencies";

    "-one-line", Cmd.Unit ignore, ": does nothing";
    "-open", String add_open, "<name>: open module <name> at the start of \
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


    "-aliases", Unit (setc pp_aliases), ": print aliases";
    "-info", Unit (setc info), ": print detailed information";
    "-export", Unit (setc export), ": export resolved modules signature";

    "-dot", Unit (setc dot), ": print dependencies in dot format";
    "-dsort", Unit(setc dsort),": print unit paths in topological order";
    "-makefile", Unit (setc makefile_c), ": print makefile depend file(default)";
    "-approx-m2l", Unit (set_iter approx_file), ": print approximated m2l ast";
    "-m2l", Unit (set_iter m2l), ": print m2l ast";
    "-m2l-sexp", Unit (set_iter m2l_sexp), ": print m2l ast in s-expression format";

    "-one-pass", Unit (set_iter one_pass), ": print m2l ast after one pass";
    "-sig", Unit (setc sign), ": print inferred signature";
    "-sig-only", set_t sig_only,
    ": filter produced m2l to keep only signature-level elements.\
     \n\n Module suboptions:\n";

    "-nl-modules", Unit (setc @@ line_modules ~filter:dep_filter),
    ": print new-line separated raw dependencies";
    "-extern-modules", Unit (setc @@ modules ~filter:lib_filter),
    ": print raw extern dependencies";
    "-inner-modules", Unit (setc @@ modules ~filter:inner_filter),
    ": print raw inner dependencies";
    "-unknown-modules", Unit (setc @@ modules ~filter:extern_filter),
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

    "-L", String lib, "<dir>: use all cmi files in <dir> \
                       in the analysis";
    "-no-alias-deps", set_t transparent_aliases, ": delay aliases dependencies";
    "-o", Cmd.String (use_p output), "<filename>: setc current output file";
    "-no-implicits", set_f implicits,
    ": do not implicitly search for a mli \
     file when given a ml file input";
    "-no-include", set_t no_include, ": do not include base directory by default";
    "-no-stdlib", set_t no_stdlib, ": do not use precomputed stdlib environment";
    "-std-otherlibs", set_t std_otherlibs,
    ": use precomputed signature for stdlib otherlibs, \
     i.e. bigarray, threads … ";
    "-read-sig", Cmd.String read_sig,
    "<signature>: add signature to the base environment";
    "-see", Cmd.String add_invisible_file,
    "<file>: use <file> in dependencies computation but do not display it.";
    "-transparent-extension-node", Cmd.Bool (use_p transparent_extension_nodes),
    "<bool>: inspect unknown extension nodes\n"
  ]

let () =
  Compenv.readenv stderr Before_args
  ; if not !param.no_include then add_include "."
  ; Cmd.parse args add_file usage_msg
  ; let libs, ppxs = Findlib.process () in
    List.iter lib libs; List.iter (Option.iter add_ppx) ppxs
  ; Compenv.readenv stderr Before_link
  ; if !action = [] then setc makefile_c ()
  ; List.iter act !action
