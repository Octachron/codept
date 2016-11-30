module Cmd = Arg
module U = Unit
module Pkg = Paths.Pkg
module Pth = Paths.Simple

open M2l
let (%) f g x = f (g x)

module S = Module.Sig
let std = Format.std_formatter

type param =
  {
    all: bool;
    native: bool;
    bytecode: bool;
    abs_path: bool;
    slash:string;
    sort:bool;
    transparent_aliases: bool;
    transparent_extension_nodes: bool;
    includes: Pkg.path Name.map;
    synonyms: Name.set Unit.pair;
    no_stdlib:bool;
    implicits: bool;
    closed_world: bool;
    fail_early:bool;
  }


type task =
  {
    files: string list Unit.pair;
    invisibles: Pth.set;
    libs: string list;
    opens: Pth.t list
  }


let lift { transparent_extension_nodes; transparent_aliases; _ } =
(module struct
    let transparent_extension_nodes = transparent_extension_nodes
    let transparent_aliases = transparent_aliases
  end
  : Interpreter.param )

let param = ref {
  all = false;
  native = false;
  bytecode = false;
  abs_path = false;
  sort = false;
  slash = Filename.dir_sep;
  transparent_aliases = false;
  transparent_extension_nodes = true;
  includes = Name.Map.empty;
  implicits = true;
  no_stdlib = false;
  synonyms = {Unit.ml = Name.Set.singleton "ml" ; mli = Name.Set.singleton "mli" };
  closed_world = false;
  fail_early = true;
}


let tool_name = "codept light"
let stderr= Format.err_formatter

let rec last = function
  | [] -> raise @@ Invalid_argument ("Empty lists do not have a last element")
  | [a] -> a
  | _ :: q -> last q

exception Unknown_file_type of string

let extension name =
  let n = String.length name in
  let r = try String.rindex name '.' with Not_found -> n-1 in
  String.sub name (r+1) (n-r-1)

let classify synonyms f =
  let ext = extension f in
  if Name.Set.mem ext synonyms.Unit.mli then
    M2l.Signature
  else if Name.Set.mem ext synonyms.ml then
    M2l.Structure
  else
    raise @@ Unknown_file_type ext

let to_m2l synonyms f =
  if extension f = "cmi" then
    Cmi.m2l f
  else
    let kind = classify synonyms f in
    match Read.file kind f with
    | _name, Ok x -> x
    | _, Error msg -> Error.syntaxerr msg

let approx_file _param f =
  let _name, lower, upper = Approx_parser.file f in
  Pp.fp std  "lower bound:%a@. upper bound:%a@."
    M2l.pp lower M2l.pp upper

let one_pass param f =
  let module Param = (val lift param) in
  let module Sg = Envts.Interpreters.Sg(Param) in
  let start = to_m2l param.synonyms f in
  match start |> Sg.m2l S.empty with
  | Ok (_state,d) -> Pp.fp std "Computation finished:\n %a@." S.pp d
  | Error h -> Pp.fp std "Computation halted at:\n %a@." M2l.pp h

let m2l param f =
  let start = to_m2l param.synonyms f in
  start
  |> Normalize.all
  |> snd
  |> Pp.fp std  "%a@." M2l.pp


let order units =
  let open Unit in
  let compute (i,m) u = i+1, Name.Map.add u.Unit.name i m in
  snd @@ List.fold_left compute (0,Name.Map.empty)
  @@ List.rev @@ List.filter (fun u -> Pkg.is_known u.path) @@ units

let topos_compare order x y =
  let get x=Name.Map.find_opt (Pkg.module_name x) order in
  match get x, get y with
  | Some k , Some l -> compare k l
  | None, Some _ -> -1
  | Some _, None -> 1
  | None, None -> compare x y

let local = Pkg.local

let open_in opens unit =
  List.fold_right (fun m (unit:Unit.t) ->
      match m with
      | [root] when unit.name = root -> unit
      | m -> { unit with code = M2l.Open m :: unit.code }
    ) opens unit

let read_mli fail_early (map,brokens) kind mli =
  let module Approx = Approx_parser.Unit in
  let module M = Unit.Groups.Unit.Map in
  match Unit.read_file kind mli with
  | Ok u -> M.add u map, brokens
  | Error msg ->
    if fail_early then
      Error.syntaxerr msg
    else
      let fiction = Approx.fiction kind mli  in
      let broken = Approx.under kind mli in
      M.add fiction map, Unit.Set.add broken brokens

let read fail_early (_arg) { Unit.ml; mli } mb =
  let module Approx = Approx_parser.Unit in
  let module M = Unit.Groups.Unit.Map in
  match mli, ml with
  | None , None -> mb
  | Some mli, Some ml ->
     let map, brokens = read_mli fail_early mb Signature mli  in
     begin match Unit.read_file Structure ml with
       | Ok ml -> M.add ml map, brokens
       | Error msg ->
         if fail_early then
           Error.syntaxerr msg
         else
         map, Unit.Set.add (Approx.under Structure ml) brokens
     end
  | None, Some mli ->
    read_mli fail_early mb Signature mli
  | Some ml, None ->
    read_mli fail_early mb Structure ml


let organize fail_early opens files =
  let add_name m n  =  Name.Map.add (Read.name n) (local n) m in
  let m = List.fold_left add_name
      Name.Map.empty (files.Unit.ml @ files.mli) in
  let grp = Unit.Groups.Filename.group files in
  let units, brokens = Paths.S.Map.fold (read fail_early) grp
      (Paths.S.Map.empty, Unit.Set.empty) in
  let units = Unit.Groups.Unit.split units in
  let units =
    Unit.unimap (List.map @@ open_in opens) units in
  units, brokens, m

let base_env no_stdlib =
  if no_stdlib then
    Envts.Base.empty
  else
    Stdlib.signature

type 'a envt_kind = (module Interpreter.envt_with_deps with type t = 'a)
type envt = E: 'a envt_kind * 'a -> envt

let start_env param  includes fileset filemap
  =
  let layered = Envts.Layered.create includes fileset @@ base_env param.no_stdlib in
  let traced = Envts.Trl.extend layered in
  if not param.closed_world then
    E ((module Envts.Tr: Interpreter.envt_with_deps with type t = Envts.Tr.t ) ,
       Envts.Tr.start traced filemap )
  else
    E ( (module Envts.Trl: Interpreter.envt_with_deps with type t = Envts.Trl.t),
        traced
      )

let remove_units invisibles =
  List.filter @@ function
    | { Unit.path = { Pkg.source=Local; file}; _ } ->
      not @@ Pth.Set.mem file invisibles
    | _ -> false

let analyze param {opens;libs;invisibles;files;_} =
  let units, _brokens, filemap = organize param.fail_early opens files in
  let files_set = units.mli |> List.map (fun u -> u.Unit.name) |> Name.Set.of_list in
  let E ((module Envt), core) = start_env param libs files_set filemap in
  let module S = Solver.Make(Envt)((val lift param)) in
  let {Unit.ml; mli} =
    try S.resolve_split_dependencies core units with
      S.Cycle (_env,units) ->
      Error.log "%a" Solver.Failure.pp_cycle units
  in
  let ml = remove_units invisibles ml in
  let mli = remove_units invisibles mli in
  { Unit.ml; mli }

let deps param task =
  let {Unit.ml; mli} = analyze param task in
  let print =  Pp.(list ~sep:(s" @,") @@ Unit.pp ) std in
  print ml; print mli

let export param task =
  let {Unit.mli; _} = analyze param task in
  let sign = function
    | {Unit.code = M2l.Defs d :: _ ; _ } ->
      d.defined
    | _ -> Module.Sig.empty in
  let md (unit:Unit.t) =
    {Module.
      name = unit.name
    ; origin = Unit { source=Pkg.Special "exported"; file = [unit.name] }
    ; args = []
    ; signature = sign unit
    } in
  let s =
    let open Module.Sig in
    List.fold_left (fun sg u -> merge sg @@ create @@ md u) empty mli
  in
  Pp.fp std "@[<hov>let signature=@;\
             let open Module in @;\
             let open Sig in @;\
             %a\
             @]@." Module.reflect_signature s

let make_abs abs p =
  let open Paths.Pkg in
  if abs && p.source = Local then
    { p with file = Sys.getcwd() :: p.file }
  else
    p

let pp_module sort {abs_path;slash; _ } ?filter ppf u =
  let pp_pkg = Pkg.pp_gen slash in
  let open Unit in
  let elts = Pkg.Set.elements u.dependencies in
  let elts = sort elts in
  let elts = match filter with
    | Some f -> List.filter f elts
    | None -> elts in
  Pp.fp ppf "%a: %a\n" pp_pkg (make_abs abs_path u.path)
    Pp.( list ~sep:(s" ") Name.pp )
    ( List.map Pkg.module_name elts)

let inner_filter = function
  | { Pkg.source = Local; _ } -> true
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
  let order = order mli in
  let compare x y = topos_compare order (proj x) (proj y) in
  if param.sort then List.sort compare
  else id


let modules ?filter param task =
  let {Unit.ml; mli} = analyze param task in
  let sort_p = sort id param mli in
  let sort_u = sort upath param mli in
  let print units = Pp.fp std "%a"
      Pp.(list ~sep:(s"") @@ pp_module sort_p param ?filter)
      (sort_u units) in
  print ml; print mli

let local_dependencies sort unit =
  sort
  @@ List.filter
    (function {Pkg.source=Unknown; _ }
            | {Pkg.source=Special _ ; _ } -> false | _ -> true )
  @@ Pkg.Set.elements unit.U.dependencies


let dot param task =
  let open Unit in
  let {mli; _ } = analyze param task in
  let sort = sort id param mli in
  Pp.fp Pp.std "digraph G {\n";
  List.iter (fun u ->
      List.iter (fun p ->
          Pp.fp std "%s -> %s \n" u.name @@ Pkg.module_name p)
        (local_dependencies sort u)
    ) mli;
  Pp.fp Pp.std "}\n"

let regroup {Unit.ml;mli} =
  let add l m = List.fold_left (fun x y -> Unit.Groups.Unit.Map.add y x) m l in
  add mli @@ add ml @@ Pth.Map.empty


let replace_deps includes unit =
  let replace = function
    | { Pkg.source = Unknown; file = [name] } as x ->
      begin
        try Name.Map.find name includes with Not_found -> x
      end
    | x -> x in
  { unit with Unit.dependencies =
                Pkg.Set.of_list
                @@ List.map replace
                @@ Pkg.Set.elements unit.Unit.dependencies }

let implicit_mli synonyms name =
    (* implicitely looks for interface files *)
      Name.Set.fold (fun ext found ->
          found ||
          Sys.file_exists @@ Filename.remove_extension
            (Pkg.filename name) ^ "." ^ ext
        )
        synonyms.Unit.mli false

let print_deps param order input dep ppf (unit,imore,dmore) =
  let unit = replace_deps param.includes unit in
  let make_abs = make_abs param.abs_path in
  let pkg_pp = Pkg.pp_gen param.slash in
  let sort = if param.sort then List.sort (topos_compare order) else id in
  let open Unit in
  let dep x= make_abs @@ dep x in
  let ppl ppf l = Pp.(list ~sep:(s" ") ~post:(s" ") pkg_pp) ppf
      (List.map make_abs l) in
  Pp.fp ppf "%a %a:%a %a\n"
    pkg_pp ( make_abs @@ input unit.path)
    ppl imore
    Pp.(list ~pre:(s " ") ~sep:(s " ") pkg_pp)
    ( List.rev_map dep
      @@ List.sort (topos_compare order)
      @@ local_dependencies sort unit
    )
    ppl dmore


let makefile param task =
  let all = param.all in
  let if_all l = if all then l else [] in
  (*  let make_abs = make_abs param.abs_path in *)
  let print_deps = print_deps param in
  let ppf = Pp.std in
  let units = analyze param task in
  let order = order units.Unit.mli in
  let m =regroup units in
  Pth.Map.iter (fun _k g ->
      let open Unit in
      match g with
      | { ml= Some impl ; mli = Some intf } ->
        let cmi = Pkg.cmi impl.path in
        if not param.native then
          print_deps order (Pkg.cmo) (Pkg.mk_dep all param.native) ppf
            (impl, [], [cmi] @ if_all [impl.path] );
        if not param.bytecode then
          print_deps order (Pkg.cmx) (Pkg.mk_dep all param.native) ppf
            (impl, if_all [Pkg.o impl.path], [cmi] @ if_all [impl.path] );
        print_deps order Pkg.cmi (Pkg.mk_dep all param.native)  ppf
          (intf,[], [] )
      | { ml = Some impl; mli = None } ->
        begin
          let cmi = Pkg.cmi impl.path in
          let imli =  param.implicits
                      && implicit_mli param.synonyms impl.path in
          let cmi_dep, cmi_adep =
            ( if imli then
                [cmi], []
              else [], [cmi] ) in
          if not param.native then
            begin
              print_deps order Pkg.cmo (Pkg.mk_dep all param.native) ppf
                (impl, if_all cmi_adep, if_all [impl.path] @ cmi_dep)
            end;
          if not param.bytecode then
            print_deps order Pkg.cmx (Pkg.mk_dep all param.native) ppf
              (impl,
               if_all ([Pkg.o impl.path] @ cmi_adep),
               if_all [impl.path] @ cmi_dep )
        end
      | { ml = None; mli = Some intf } ->
        print_deps order Pkg.cmi (Pkg.mk_dep all param.native) ppf
          (intf,[],[])
      | { ml = None; mli = None } -> ()
    ) m


let task = ref {
    files = { Unit.ml = []; Unit.mli = [] };
    invisibles = Pth.Set.empty;
    libs = [];
    opens = []
  }

let add_invi name =
  task := { !task with
            invisibles = Pth.Set.add (Paths.S.parse_filename name) (!task).invisibles
          }

let add_impl name =
  let {Unit.ml;mli} = (!task).files in
  task := { !task with files = { ml = name :: ml; mli } }

let add_intf name =
  let {Unit.ml;mli} = !(task).files in
  task := {!task with files = { mli = name :: mli; ml } }

let add_file name =
  if Sys.file_exists name then
    match classify !param.synonyms name with
    | M2l.Structure ->
      add_impl name
    | Signature -> add_intf name

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
  let synonyms = !param.synonyms in
  let synonyms =  { synonyms with ml = Name.Set.add s synonyms.ml } in
  param := { !param with synonyms }


let mli_synonym s =
  let synonyms = !param.synonyms in
  let synonyms = { synonyms with mli = Name.Set.add s synonyms.mli } in
  param := { !param with synonyms }


let lib f =
  task := { !task with libs = f :: (!task).libs }

let action = ref ignore
let set command () = action:= (fun () -> command !param !task)
let () = set makefile ()

let set_iter command () = action := begin
    fun () ->
      let {Unit.ml;mli} = (!task).files  in
      List.iter (command !param) (ml @ mli)
  end

let transparent_aliases value =
  param := { !param with transparent_aliases = value }

let transparent_extension value =
  param:= { !param with transparent_extension_nodes = value }

let version = 0.01
let print_vnum ()= Format.printf "%.2f@." version
let print_version ()= Format.printf "codept, version %.2f@." version

let abs_path () =
  param := { !param with abs_path = true }

let all () =
  param := { !param with all = true }

let no_stdlib () =
   param := { !param with no_stdlib = true }

let native () =
  param := { !param with native = true; bytecode = false }

let bytecode () =
  param := { !param with bytecode = true; native = false }


let map file =
  transparent_aliases true;
  add_invisible_file file

let as_map file =
  transparent_aliases true;
  add_file file

let slash () =
  param := { !param with slash = "/" }

let sort () =
  param := { !param with sort = true }

let close_world () =
    param := { !param with closed_world = true }


let allow_approx () =
  param := { !param with fail_early = false }

let pkg name =
  let cmd = "ocamlfind query " ^ name in
  let cin = Unix.open_process_in cmd in
  try
    let result = input_line cin in
    lib result
  with
  End_of_file -> ()

let add_include dir =
  let files = Sys.readdir dir in
  let dir = if dir = "." then [] else Paths.S.parse_filename dir in
  let includes =
    Array.fold_left (fun m x ->
        match classify !param.synonyms x with
        | exception Unknown_file_type _ -> m
        | _ ->
          Name.Map.add (Read.name x)
            Pkg.( dir / local x) m
      )
      !param.includes files
  in
  param :=
    { !param with includes }

let no_implicits () =
  param := { !param with implicits = false }

let usage_msg =
  "Codept is an alternative dependency solver for OCaml.\n\
   Usage: codept [options] ⟨source files⟩.\n\
   The following options are common with ocamldep:\n"


let args = Cmd.[
    "-absname", Cmd.Unit abs_path, ": use absolute path name";
    "-all", Unit all, ": display full dependencies in makefile";
    "-allow-approx", Unit allow_approx,": fall back to approximated parser \
                                        in presence of syntax errors.";
    "-as-map", Cmd.String as_map, "<file>: same as \
                                   \"-no-alias-deps <file>\"";
    "-I", String add_include,"<dir>: do not filter files in <dir> when printing \
                          dependencies";
    "-impl", String add_impl, "<f>: read <f> as a ml file";
    "-intf", String add_intf, "<f>: read <f> as a mli file";
    "-map", Cmd.String map, "<file>: same as \"-no-alias-deps \
                            -see <file>\"";
    "-ml-synonym", String ml_synonym, "<s>: use <s> extension as a synonym \
                                       for ml";
    "-mli-synonym", String ml_synonym, "<s>: use <s> extension as a synonym \
                                        for mli";
    "-modules", Unit (set modules), ": print raw module dependencies";
    "-native", Cmd.Unit native, ": generate native compilation only dependencies";
    "-bytecode", Cmd.Unit bytecode, ": generate bytecode only dependencies";

    "-one-line", Cmd.Unit ignore, ": does nothing";
    "-open", String add_open, "<name>: open module <name> at the start of \
                               all compilation units \n\
                               (except units whose name is <name>).";
    "-pp", Cmd.String(fun s -> Clflags.preprocessor := Some s),
    "<cmd>: pipe sources through preprocessor <cmd>";
    "-ppx", Cmd.String add_ppx,
    "<cmd>: pipe abstract syntax trees through ppx preprocessor <cmd>";
    "-slash", Cmd.Unit slash, ": use forward slash as directory separator";
    "-sort", Cmd.Unit slash, ": sort dependencies when printing";
    "-version", Cmd.Unit print_version,
    ": print human-friendly version description";
    "-vnum", Cmd.Unit print_vnum, ": print version number\n\n Codept only modes:\n";


    "-deps", Unit (set deps), ": print detailed dependencies";
    "-export", Unit (set export), ": export resolved modules signature";

    "-dot", Unit (set dot), ": print dependencies in dot format";
    "-makefile", Unit (set makefile), ": print makefile depend file(default)";
    "-approx-m2l", Unit (set_iter approx_file), ": print approximated m2l ast:";
    "-m2l", Unit (set_iter m2l), ": print m2l ast:";
    "-one-pass", Unit (set_iter one_pass), ": print m2l ast after one pass\
                                            \n\n Module suboptions:\n";

    "-extern-modules", Unit (set @@ modules ~filter:lib_filter),
    ": print raw extern dependencies";
    "-inner-modules", Unit (set @@ modules ~filter:inner_filter),
    ": print raw inner dependencies";
    "-unknown-modules", Unit (set @@ modules ~filter:extern_filter),
    ": print raw unresolved dependencies\n\n Misc options:\n";
    "-closed-world", Unit close_world,
    ": require that all dependencies are provided";

    "-L", String lib, "<dir>: use all cmi files in <dir> \
                               in the analysis";
    "-no-alias-deps", Cmd.Unit (fun () -> transparent_aliases true),
    ": Delay aliases dependencies";
    "-no-implicits", Cmd.Unit no_implicits,
    ": do not implicitly search for a mli \
     file when given a ml file input";
    "-no-stdlib", Cmd.Unit no_stdlib,
    ": do not use precomputed stdlib environment";
    "-pkg", Cmd.String pkg, "<pkg_name>: use the ocamlfind package <pkg_name> \
                             during the analysis";
    "-see", Cmd.String add_invisible_file, "<file>: use <file> in dependencies \
                                            computation but do not display it.";
    "-transparent_extension_node", Cmd.Bool transparent_extension,
    "<bool>: inspect unknown extension nodes"
  ]

let () =
  Compenv.readenv stderr Before_args
  ; add_include "."
  ; Cmd.parse args add_file usage_msg
  ; Compenv.readenv stderr Before_link
  ; !action ()
