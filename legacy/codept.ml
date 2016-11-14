module Cmd = Arg
module U = Unit
module Pkg = Package

open M2l

module S = Module.Sig
let std = Format.std_formatter

type param =
  {
    mutable all: bool;
    mutable native: bool;
    mutable abs_path: bool;
    mutable transparent_aliases: bool;
    mutable transparent_extension_nodes: bool
  }

type task =
  {
    files: string list Unit.split;
    invisibles: Npath.set;
    includes: string list;
    opens: Npath.t list
  }


let lift { transparent_extension_nodes; transparent_aliases; _ } =
(module struct
    let transparent_extension_nodes = transparent_extension_nodes
    let transparent_aliases = transparent_aliases
  end
  : Interpreter.param )

let param = {
  all = false;
  native = false;
  abs_path = false;
  transparent_aliases = true;
  transparent_extension_nodes = true
}


let tool_name = "codept legacy"
let stderr= Format.err_formatter

let rec last = function
  | [] -> raise @@ Invalid_argument ("Empty lists do not have last element")
  | [a] -> a
  | _ :: q -> last q

exception Unknown_file_type of string

let extension name =
  let n = String.length name in
  let r = try String.rindex name '.' with Not_found -> n-1 in
  String.sub name (r+1) (n-r-1)

let to_m2l f =
  match extension f with
  | "ml" ->
    f
    |> open_in |> Lexing.from_channel
    |> Parse.implementation |> Ast_analyzer.structure
  | "mli" ->
    f
    |> open_in |> Lexing.from_channel
    |> Parse.interface |> Ast_analyzer.signature
  | "cmi" ->
    Cmi.cmi_m2l f
  | ext -> raise (Unknown_file_type ext)

let one_pass f =
  let module Param = (val lift param) in
  let module Sg = Envts.Interpreters.Sg(Param) in
  let start = to_m2l f in
  match start |> Sg.m2l S.empty with
  | Done (_state,d) -> Pp.fp std "Computation finished:\n %a@." S.pp d
  | Halted h -> Pp.fp std "Computation halted at:\n %a@." M2l.pp h

let m2l f =
  let start = to_m2l f in
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

let local file = Pkg.(local @@ parse_filename file)

let open_in opens unit =
  List.fold_right (fun m (unit:Unit.t) ->
      match m with
      | [root] when unit.name = root -> unit
      | m -> { unit with code = M2l.Open m :: unit.code }
    ) opens unit

let organize opens files =
  let add_name m n  =  Name.Map.add (Unit.extract_name n) (local n) m in
  let m = List.fold_left add_name
      Name.Map.empty (files.Unit.ml @ files.mli) in
  let units = Unit.( split @@ group files ) in
  let units =
    let f = List.map (open_in opens) in
    { Unit.ml = f units.ml; mli = f units.mli }
  in
  units, m

let start_env includes filemap =
  Envts.Tr.start Envts.(
      Trl.extend @@ Layered.create includes
      @@ Envts.Base.empty) filemap

let remove_units invisibles =
  List.filter @@ function
    | { Unit.path = { Pkg.source=Local; file}; _ } ->
      not @@ Npath.Set.mem file invisibles
    | _ -> false

let analyze {opens;includes;invisibles;files} =
  let units, filemap = organize opens files in
  let module Envt = Envts.Tr in
  let core = start_env includes filemap in
  let module S = Solver.Make((val lift param)) in
  let {Unit.ml; mli} =
    try S.resolve_split_dependencies core units with
      S.Cycle (env,units) ->
      Error.log "%a@;Env:@ %a"
        Solver.Failure.pp_cycle units
        Module.pp_signature env.core.env.local
  in
  let ml = remove_units invisibles ml in
  let mli = remove_units invisibles mli in
  { Unit.ml; mli }

let deps task =
  let {Unit.ml; mli} = analyze task in
  let print =  Pp.(list ~sep:(s" @,") @@ Unit.pp ) std in
  print ml; print mli

let make_abs p =
  let open Package in
  if param.abs_path && p.source = Local then
    { p with file = Sys.getcwd() :: p.file }
  else
    p



let pp_module ?filter ppf u =
  let open Unit in
  let elts = Pkg.Set.elements u.dependencies in
  let elts = match filter with
    | Some f -> List.filter f elts
    | None -> elts in
  Pp.fp ppf "%a: %a\n" Pkg.pp (make_abs u.path)
    Pp.( list ~sep:(s" ") Name.pp )
    ( List.map Pkg.module_name elts)

let inner_filter = function
  | { Pkg.source = Local; _ } -> true
  |  _ -> false

let extern_filter = function
  | { Pkg.source = Unknown; _ } -> true
  | _ -> false

let lib_filter = function
  | { Pkg.source = Pkg _ ; _ } -> true
  | _ -> false


let modules ?filter task =
  let {Unit.ml; mli} = analyze task in
  let print units = Pp.(list @@ pp_module ?filter) std
      (List.sort Unit.(fun x y -> compare x.path.file y.path.file) units) in
  print ml; print mli

let local_dependencies unit =
  List.filter (function {Pkg.source=Unknown;_} -> false | _ -> true )
  @@ Pkg.Set.elements unit.U.dependencies


let dot task =
  let open Unit in
  let {mli; _ } = analyze task in
  Pp.fp Pp.std "digraph G {\n";
  List.iter (fun u ->
      List.iter (fun p ->
          Pp.fp std "%s -> %s \n" u.name @@ Pkg.module_name p)
        (local_dependencies u)
    ) mli;
  Pp.fp Pp.std "}\n"

let regroup {Unit.ml;mli} =
  let add l m = List.fold_left (fun x y -> Unit.Group.Map.add y x) m l in
  add mli @@ add ml @@ Npath.Map.empty

let print_deps order input dep ppf (unit,imore,dmore) =
  let if_all l = if param.all then l else [] in
  let open Unit in
  let dep x= make_abs @@ dep x in
  let ppl ppf l = Pp.(list ~sep:(s" ") Pkg.pp) ppf (List.map make_abs l) in
  Pp.fp ppf "%a %a:%a %a\n"
    Pkg.pp ( make_abs @@ input unit.path)
    ppl (if_all imore)
    Pp.(opt_list_0 ~pre:(s " ") ~sep:(s " ") Pkg.pp)
    ( List.map dep
      @@ List.sort (topos_compare order)
      @@ local_dependencies unit
    )
    ppl (if_all dmore)

let makefile task =
  let all = param.all in
  let all_cons x l =
    List.map make_abs @@ if all then x :: l else [x] in
  let ppl ppf (x,l) = Pp.(list ~sep:(s" ") Pkg.pp) ppf (all_cons x l) in
  let ppf = Pp.std in
  let units = analyze task in
  let order = order units.Unit.mli in
  let m = regroup units in
  Npath.Map.iter (fun _k g ->
      let open Unit.Group in
      match g with
      | { impl= Some impl ; intf = Some intf } ->
        if not param.native then
          Pp.fp ppf "%a : %a\n"
            Pkg.pp ( make_abs @@ Pkg.cmo impl.path)
            ppl  (Pkg.cmi impl.path, [impl.path]);
        Pp.fp ppf "%a : %a\n"
          ppl (Pkg.cmx impl.path, [Pkg.o impl.path])
          ppl  (Pkg.cmi impl.path, [impl.path]);
        print_deps order Pkg.cmi (Pkg.mk_dep param.native) ppf
          (intf,[],[])
      | { impl = Some impl; intf = None } ->
        begin
          if not param.native then
            print_deps order Pkg.cmo (Pkg.mk_dep param.native) ppf
              (impl,[Pkg.cmi impl.path],[impl.path]);
          print_deps order Pkg.cmx (Pkg.mk_dep param.native) ppf
            (impl,[Pkg.o impl.path; Pkg.cmi impl.path],[impl.path])
        end
      | { impl = None; intf = Some intf } ->
        print_deps order Pkg.cmi (Pkg.mk_dep param.native) ppf
          (intf,[],[])
      | { impl = None; intf = None } -> ()
    ) m


let usage_msg = "codept is an alternative dependencies solver for OCaml"

let ml_synonyms = ref @@ Name.Set.singleton "ml"
let mli_synonyms =  ref @@ Name.Set.singleton "mli"

let classify f =
  let ext = extension f in
  if Name.Set.mem ext !mli_synonyms then
    Unit.Signature
  else if Name.Set.mem ext !ml_synonyms then
    Unit.Structure
  else
    raise @@ Unknown_file_type ext

let task = ref {
    files = { Unit.ml = []; Unit.mli = [] };
    invisibles = Npath.Set.empty;
    includes = [];
    opens = []
  }

let add_impl name =
  let {Unit.ml;mli} = (!task).files in
  task := { !task with files = { ml = name :: ml; mli } }

let add_intf name =
  let {Unit.ml;mli} = !(task).files in
  task := {!task with files = { mli = name :: mli; ml } }

let add_file name =
  match classify name with
    | Structure -> add_impl name
    | Signature -> add_intf name

let add_invisible_file name =
  task := { !task with
            invisibles = Npath.Set.add (Pkg.parse_filename name) (!task).invisibles
          };
  add_file name

let add_open name =
  task := { !task with opens = [name] :: (!task).opens }

let first_ppx = Compenv.first_ppx

let add_ppx ppx =
  first_ppx := ppx :: !first_ppx

let ml_synonym s =
   mli_synonyms := Name.Set.add s !mli_synonyms

let mli_synonym s =
  mli_synonyms := Name.Set.add s !mli_synonyms

let include_ f =
  task := { !task with includes = f :: (!task).includes }

let action = ref ignore
let set command () = action:= (fun () -> command !task)
let () = set makefile ()

let set_iter command () = action := begin
    fun () ->
      let {Unit.ml;mli} = (!task).files  in
      List.iter command (ml @ mli)
  end

let transparent_aliases value = param.transparent_aliases <- value
let transparent_extension value = param.transparent_extension_nodes <- value

let version = 0.01
let print_vnum ()= Format.printf "%.2f@." version
let print_version ()= Format.printf "codept, version %.2f@." version

let abs_path () = param.abs_path <- true
let all () = param.all <- true
let native () = param.native <- true

let map file =
  transparent_aliases true;
  add_invisible_file file

let as_map file =
  transparent_aliases true;
  add_file file

let args = Cmd.[
    "-all", Unit all, ":   display full dependencies in makefile";
    "-modules", Unit (set modules), ":   print raw module dependencies";
    "-unknown-modules", Unit (set @@ modules ~filter:extern_filter),
    ":   print raw unresolved dependencies";
    "-inner-modules", Unit (set @@ modules ~filter:inner_filter),
    ":   print raw inner dependencies";
    "-extern-modules", Unit (set @@ modules ~filter:lib_filter),
    ":   print raw extern dependencies";
    "-deps", Unit (set deps), ": print detailed dependencies";
    "-m2l", Unit (set_iter m2l), ":   print m2l ast";
    "-impl", String add_impl, "<f>:   read <f> as a ml file";
    "-intf", String add_intf, "<f>:   read <f> as a mli file";
    "-ml-synonym", String ml_synonym, "<s>:   use <s> extension as a synonym \
                                       for ml";
    "-mli-synonym", String ml_synonym, "<s>:   use <s> extension as a synonym \
                                        for mli";
    "-one-pass", Unit (set_iter one_pass), ":   print m2l ast after one pass";
    "-makefile", Unit (set makefile), ":   print makefile depend file";
    "-dot", Unit (set dot), ":   print dependencies in dot format";
    "-I", String include_, "<dir>:   include <dir> in the analyssi all cmi files\
                            in <dir>";
    "-open", String add_open, "<name>:   open module <name> at the start of \
                               all compilation units\
                               (except units whose name is <name>).";
    "-pp", Cmd.String(fun s -> Clflags.preprocessor := Some s),
    "<cmd>:   Pipe sources through preprocessor <cmd>";
    "-ppx", Cmd.String add_ppx,
    "<cmd>:   Pipe abstract syntax trees through ppx preprocessor <cmd>";
    "-transparent_extension_node", Cmd.Bool transparent_extension,
    "<bool>:   Inspect unknown extension nodes";
    "-transparent_aliases", Cmd.Bool transparent_aliases,
    "<bool>:   Delay aliases dependencies";
    "-vnum", Cmd.Unit print_vnum, "print version number";
    "-version", Cmd.Unit print_version,
    "print human-friendly version description";
    "-as-map", Cmd.String as_map, "<file>:   same as \
                                   -file <file> -transparent-aliases true";
    "-map", Cmd.String map, "<file>: same as -transparent-aliases true \
                            -see <file>";
    "-abs-path", Cmd.Unit abs_path, ":   use absolute path name";
    "-native", Cmd.Unit native, ":   generate native compilation only dependencies";
    "-one-line", Cmd.Unit ignore, ":   does nothing";
    "-see", Cmd.String add_invisible_file, "<file>:   use <file> in dependencies\
                                            computation but do not display it."
  ]

let () =
  Compenv.readenv stderr Before_args
  ; Cmd.parse args add_file usage_msg
  ; Compenv.readenv stderr Before_link
  ; !action ()