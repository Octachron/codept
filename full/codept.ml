module Cmd = Arg
module U = Unit
module Pkg = Paths.Pkg
module Pth = Paths.Simple

(** Utility functions and module *)
open M2l
let (%) f g x = f (g x)
let tool_name = "codept light"
let stderr= Format.err_formatter

module S = Module.Sig
let std = Format.std_formatter


module Self_polycy = struct
  open Fault
  let polycy = Fault.Polycy.default
  let unknown_extension =
    { path = ["codept"; "io"; "unknown extension"];
      expl = "Codept fault: attempting to read a file with an unknwon extension";
      log = (fun lvl -> log lvl "Attempting to read %s, aborting due to \
                                 an unknown extension.")
    }


    let m2l_syntaxerr =
    { path = ["codept"; "parsing"; "m2l"];
      expl = "Parsing fault: syntax error when parsing a m2l serialized file.";
      log = (fun lvl -> log lvl
                "Parsing fault: syntax error when parsing the m2l serialized \
                 file %s."
            )
    }

    let solver_error =
      { path = ["resolving"; "blocker" ];
        expl = "Solver fault: major errors during analysis.";
        log = (fun lvl -> log lvl
                  "Solver failure@?@[@<2> @[<0>@;%a@]@]" Solver.Failure.pp_cycle
              )
      }

  let polycy =
    let open Polycy in
    polycy
    |> set_err (unknown_extension, Level.warning)
    |> set_err (m2l_syntaxerr, Level.warning)
    |> set_err (solver_error, Level.error)

  let parsing_approx = let open Polycy in
    polycy |> set_err (syntaxerr, Level.warning)

  let lax = { parsing_approx with exit = Level.critical }

  let quiet = { lax with silent = Level.error }

  let strict = let open Polycy in
      { polycy with exit = Level.notification }
      |> set (["typing"], Some "Typing faults", Level.error)
      |> set_err (applied_structure, Level.error)
      |> set_err (structure_expected, Level.error)

end

module Resource = struct
  type kind = Interface | Implementation | Signature
  type info = { format: Read.format; kind : kind }

let classic {format;kind}: Read.kind option = match kind with
  | Interface -> Some { format; kind = M2l.Signature }
  | Implementation -> Some { format; kind = M2l.Structure }
  | Signature -> None

let ml = { format=Src; kind = Implementation }
let mli = { format=Src; kind = Interface }


end

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
    synonyms: Resource.info Name.Map.t;
    no_include:bool;
    no_stdlib:bool;
    implicits: bool;
    closed_world: bool;
    may_approx:bool;
    sig_only:bool;
    polycy: Fault.Polycy.t;
    output: string;
  }


type task =
  {
    files: (Read.kind * string) list Unit.pair;
    signatures: Module.t list;
    invisibles: Pth.set;
    libs: string list;
    opens: Pth.t list
  }


let lift { polycy; transparent_extension_nodes; transparent_aliases; _ } =
  (module struct
    let polycy = polycy
    let transparent_extension_nodes = transparent_extension_nodes
    let transparent_aliases = transparent_aliases
  end
  : Interpreter.param )

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
  no_include = false;
  synonyms;
  closed_world = false;
  may_approx = false;
  sig_only = false;
  polycy = Self_polycy.polycy;
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
    Fault.handle polycy Self_polycy.unknown_extension ext; None

(** Printing directly from source file *)
let to_m2l polycy sig_only (k,f) =
    match Read.file k f with
    | _name, Ok x ->
      if sig_only then Some (M2l.Sig_only.filter x) else Some x
    | _, Error (Ocaml msg) -> Fault.handle polycy Fault.syntaxerr msg; None
    | _, Error M2l -> Fault.handle polycy Self_polycy.m2l_syntaxerr f; None


let approx_file ppf  _param (_,f) =
  let _name, lower, upper = Approx_parser.file f in
  Pp.fp ppf  "lower bound:%a@. upper bound:%a@."
    M2l.pp lower M2l.pp upper

let one_pass ppf param ( (_,filename) as f) =
  let module Param = (val lift param) in
  let module Sg = Interpreter.Make(Envts.Base)(Param) in
  let start = to_m2l param.polycy param.sig_only f in
  match Option.( start >>| Sg.m2l (Pkg.local filename) Envts.Base.empty ) with
  | None -> ()
  | Some (Ok (_state,d)) -> Pp.fp ppf "Computation finished:\n %a@." S.pp d
  | Some (Error h) -> Pp.fp ppf "Computation halted at:\n %a@." M2l.pp h

let m2l ppf param f =
  let start = to_m2l param.polycy param.sig_only f in
  let open Option in
  start
  >>| Normalize.all
  >>| snd
  >>| Pp.fp ppf  "%a@." M2l.pp
  >< ()

let m2l_sexp ppf param f =
  let start = to_m2l param.polycy param.sig_only f in
  let open Option in
  start
  >>| Normalize.all
  >>| snd
  >>| M2l.sexp.embed
  >>| Pp.fp ppf  "%a@." Sexp.pp
  >< ()

(** Topological order functions *)
let full_topological_sort deps paths =
  let visited = Hashtbl.create 17 in
  let mark x = Hashtbl.add visited x true in
  let is_visited x = Hashtbl.mem visited x in
  let rec sort sorted = function
    | [] -> sorted
    | a :: q ->
      if is_visited a then
        sort sorted q
      else
        let sorted = sort_at sorted a in
        sort sorted q
  and sort_at sorted x =
    let sorted = Paths.Pkg.Set.fold sort_dep (deps x) sorted in
    mark x;
    x :: sorted
  and sort_dep y sorted =
    if is_visited y then
      sorted
    else
      sort_at sorted y in
  List.rev @@ sort [] paths

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

let toposort order m =
    List.sort (fun x y -> topos_compare order (m x) (m y))

(** Basic files reading *)
let local = Pkg.local

let open_within opens unit =
  List.fold_right (fun m (unit:Unit.s) ->
      match m with
      | [root] when unit.name = root -> unit
      | m -> { unit with code = (M2l.Build.ghost @@ M2l.Open m) :: unit.code }
    ) opens unit

let organize polycy sig_only opens files =
  let add_name m (_,n)  =  Name.Map.add (Read.name n) (local n) m in
  let m = List.fold_left add_name
      Name.Map.empty (files.Unit.ml @ files.mli) in
  let filter_m2l (u: Unit.s) = if sig_only then
      { u with Unit.code = M2l.Sig_only.filter u.code }
    else
      u in
  let units =
    Unit.unimap (List.map @@ fun (info,f) -> Unit.read_file polycy info f )
      files in
  let units = Unit.unimap (List.map @@ filter_m2l % open_within opens) units in
  let units = Unit.Groups.Unit.(split % group) units in
  units, m

let base_env signatures no_stdlib =
  let start =
    if no_stdlib then
      Envts.Base.empty
    else
      Envts.Base.start Stdlib.signature in
  List.fold_left Envts.Base.add_unit start signatures

type 'a envt_kind = (module Interpreter.envt_with_deps with type t = 'a)
type envt = E: 'a envt_kind * 'a -> envt

let start_env param signatures includes fileset filemap
  =
  let base = base_env signatures param.no_stdlib in
  let layered = Envts.Layered.create includes fileset base in
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


(** Solver step *)
let solve param (E((module Envt), core)) (units: _ Unit.pair) =
  let module S = Solver.Make(Envt)((val lift param)) in
  let rec solve_harder state =
    match S.resolve_dependencies ~learn:true state with
    | Ok (e,l) -> e, l
    | Error state ->
      Fault.handle param.polycy Self_polycy.solver_error state.pending;
      solve_harder @@ S.approx_and_try_harder state in
  let env, mli = solve_harder @@ S.start core units.mli in
  let _, ml = solve_harder @@ S.start env units.ml in
  {Unit.ml;mli}

(** Analysis step *)
let analyze param {opens;libs;invisibles; signatures; files;_} =
  let units, filemap = organize param.polycy param.sig_only opens files in
  let files_set = units.mli
                  |> List.map (fun (u:Unit.s) -> u.name)
                  |> Name.Set.of_list in
  let e = start_env param signatures libs files_set filemap in
  let {Unit.ml; mli} = solve param e units in
  let ml = remove_units invisibles ml in
  let mli = remove_units invisibles mli in
  {Unit.ml;mli}

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

let sig_only () =
  param := { !param with sig_only = true }

let make_abs abs p =
  let open Paths.Pkg in
  if abs && p.source = Local then
    { p with file = Sys.getcwd() :: p.file }
  else
    p

let dependencies ?filter sort (u:Unit.r) =
  Pkg.Set.elements u.dependencies
  |> sort
  |> (match filter with
      | Some f -> List.filter f
      | None -> fun x -> x
    )
  |> List.map Pkg.module_name


let aliases (x:Unit.r) = Module.(aliases @@ M (create "" x.signature) )


let pp_module {abs_path;slash; _ } proj ppf (u:Unit.r) =
  let pp_pkg = Pkg.pp_gen slash in
  let elts = proj u in
  Pp.fp ppf "%a: %a\n" pp_pkg (make_abs abs_path u.path)
    Pp.( list ~sep:(s" ") Name.pp )
    elts


let pp_aliases ppf param task =
  let pp_pkg = Pkg.pp_gen param.slash in
  let pp_m m =
    let open Module in
    match m with
    | M { origin = Unit path; _ } as m ->
      let path' = Pkg.update_extension
          (function "m2l" -> ".ml" | "m2li" -> ".mli" | s -> s ) path in
      let f = make_abs param.abs_path path' in
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
  let order = order mli in
  if param.sort then toposort order proj
  else id

let gen_modules proj ppf param task =
  let {Unit.ml; mli} = analyze param task in
  let sort_p = sort id param mli in
  let sort_u = sort upath param mli in
  let print units = Pp.fp ppf "%a"
      Pp.(list ~sep:(s"") @@ pp_module param @@ proj sort_p)
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

let regroup {Unit.ml;mli} =
  let add l m = List.fold_left (fun x y -> Unit.Groups.R.Map.add y x) m l in
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

let implicit_dep synonyms path =
  (* implicitely looks for interface/implementation files.
     TODO: allow separated pair of .ml/.mli
  *)
  let exists ext =  Sys.file_exists @@ Filename.remove_extension
      (Pkg.filename path) ^ "." ^ ext in
  Name.Map.fold (fun ext (info:Resource.info) (found:bool Unit.pair) ->
      match info.kind with
      | Interface ->
        { found with mli = found.mli || exists ext }
      | Implementation ->
        { found with ml = found.ml || exists ext }
      | _ -> found
    )
        synonyms {ml=false;mli=false}


let print_deps param order input dep ppf (unit,imore,dmore) =
  let unit = replace_deps param.includes unit in
  let make_abs = make_abs param.abs_path in
  let pkg_pp = Pkg.pp_gen param.slash in
  let sort = if param.sort then toposort order id else id in
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


let makefile ppf param task =
  let all = param.all in
  let if_all l = if all then l else [] in
  (*  let make_abs = make_abs param.abs_path in *)
  let print_deps = print_deps param in
  let units = analyze param task in
  let order = order units.Unit.mli in
  let m =regroup units in
  let cmi_or or_ path =
    let open Unit in
    match implicit_dep param.synonyms path with
    | exception Not_found -> or_ path
    | { ml = true; mli = true } | { ml = false; mli=false } ->
        or_ path
    | { mli = false; ml = true } ->
      or_ path
    | { mli = true; ml = false } ->
      Pkg.cmi path in
  Pth.Map.iter (fun _k g ->
      let open Unit in
      match g with
      | { ml= Some impl ; mli = Some intf } ->
        let cmi = Pkg.cmi impl.path in
        if not param.native then
          print_deps order (Pkg.cmo) (cmi_or Pkg.cmo) ppf
            (impl, [], [cmi] @ if_all [impl.path] );
        if not param.bytecode then
          print_deps order (Pkg.cmx) (cmi_or Pkg.cmx) ppf
            (impl, if_all [Pkg.o impl.path], [cmi] @ if_all [impl.path] );
        print_deps order Pkg.cmi (Pkg.mk_dep all param.native)  ppf
          (intf,[], [] )
      | { ml = Some impl; mli = None } ->
        begin
          let implicit = implicit_dep param.synonyms impl.path in
          let cmi = Pkg.cmi impl.path in
          let imli =  param.implicits
                      && implicit.mli in
          let cmi_dep, cmi_adep =
            ( if imli then
                [cmi], []
              else [], [cmi] ) in
          if not param.native then
            begin
              print_deps order Pkg.cmo (cmi_or Pkg.cmo) ppf
                (impl, if_all cmi_adep, if_all [impl.path] @ cmi_dep)
            end;
          if not param.bytecode then
            print_deps order Pkg.cmx (cmi_or Pkg.cmx) ppf
              (impl,
               if_all ([Pkg.o impl.path] @ cmi_adep),
               if_all [impl.path] @ cmi_dep )
        end
      | { ml = None; mli = Some intf } ->
        print_deps order Pkg.cmi (Pkg.mk_dep all param.native) ppf
          (intf,[],[])
      | { ml = None; mli = None } -> ()
    ) m

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
  full_topological_sort deps paths
  |> Pp.list ~sep:Pp.(s" ") ~post:Pp.(s"\n") Pkg.pp ppf

let task = ref {
    files = { Unit.ml = []; Unit.mli = [] };
    signatures = [];
    invisibles = Pth.Set.empty;
    libs = [];
    opens = [];
  }

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
    match classify !param.polycy !param.synonyms name with
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
  let synonyms = !param.synonyms in
  let info = { Resource.format = Src; kind = Implementation } in
  let synonyms =   Name.Map.add s info synonyms in
  param := { !param with synonyms }


let mli_synonym s =
  let synonyms = !param.synonyms in
  let info = { Resource.format = Src; kind = Interface } in
  let synonyms =   Name.Map.add s info synonyms in
  param := { !param with synonyms }

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


let set command () =
  action:= (!param.output, fun out -> command out !param !task) :: !action

let set_iter command () = action :=
    ( !param.output,
      begin
        fun out ->
          let {Unit.ml;mli} = (!task).files  in
          List.iter (command out !param) (ml @ mli)
      end
    ) :: !action

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

let o s =
  param := { !param with output = s }

let allow_approx () =
  param := { !param with polycy = Fault.Polycy.parsing_approx }

let keep_going () =
  param := { !param with polycy = Self_polycy.lax }

let quiet () =
  param := { !param with polycy = Self_polycy.quiet }


let strict () =
  param := { !param with polycy = Self_polycy.strict }

module Findlib = struct

  type info =
    { pkgs: Name.t list;
      predicates: string list;
      syntaxes: Name.t list;
      ppxopts: string list Name.map;
      ppopt: string list
    }

let run cmd =
  let cin = Unix.open_process_in cmd in
  let rec read l =
  try
    match input_line cin with
    | "" -> read l
    | s -> read (s::l)
  with
    End_of_file -> List.rev l in
  read []

let run_word cmd = match run cmd with
  | [a] -> Some a
  | _ -> None

let query q = run@@ String.concat " " ("ocamlfind query -r " :: q)
let printppx q =
  let s = run_word @@ String.concat " " ("ocamlfind printppx " :: q) in
  Option.fmap (fun s -> String.sub s 4 @@ String.length s - 4) s

let archive q = run @@ String.concat " " ("ocamlfind query -format %a":: q)

let find_pred info =
  let p = String.concat "," info.predicates in
  if p = "" then [] else
    ["-predicates"; p ]

let camlp4 = "camlp4"
let filter predicates syntax pkg =
  archive @@ (pkg :: predicates) @ ["-pp"; "-predicates"; syntax] <> []

let find_pp info syntax pkgs =
  let predicates = find_pred info in
  let g = List.filter (filter predicates syntax) pkgs in
  let main_pp = camlp4 in
  let includes l i =
    List.fold_left (fun acc x ->  "-I" :: x :: acc ) l @@ query (i::predicates) in
  let i = List.fold_left includes [] g in
  main_pp :: i
  @ archive ( main_pp :: predicates @ ["-pp"; "-predicates"; syntax]
              @ g )
  @ List.rev info.ppopt

let process_pkg info name =
  let predicates = find_pred info in
  let dirs = query @@  predicates @ [name] in
  let ppxopt = Option.default [] @@ Name.Map.find_opt name info.ppxopts in
  let ppx = Option.fmap (fun s -> String.concat " " @@ s :: ppxopt) @@
    printppx @@ predicates @ [name] in
  List.iter lib dirs;
  Option.iter add_ppx ppx

let pkg info pkg = { info with pkgs = pkg :: info.pkgs }
let syntax info syntax = { info with syntaxes = syntax :: info.syntaxes }
let ppxopt info opt =
  match String.split_on_char ',' opt with
  | [] | [_] -> info
  | a :: q ->
    let q = String.concat "," q in
    let m = info.ppxopts in
    let q = Option.( Name.Map.find_opt a m >>| (List.cons q) >< [q] ) in
    let m = Name.Map.add a q m in
    { info with ppxopts = m }

let ppopt info opt = { info with ppopt = opt :: info.ppopt }

let predicates info s =
  let l = List.map String.trim @@ String.split_on_char ',' s in
  { info with predicates = l @ info.predicates }

let info = ref
    {
      pkgs = [];
      syntaxes =[];
      ppopt = [];
      ppxopts = Name.Map.empty;
      predicates = []
    }

let update f s = info := f !info s

let process_pp info name =
  try
    let s = find_pp info name info.pkgs in
    let s = String.concat " " s in
    Clflags.preprocessor := Some s
  with Not_found -> ()

let process () =
  let info = !info in
  let syntaxes = Name.Set.of_list (info.syntaxes) in
  Name.Set.iter (process_pp info) syntaxes;
  List.iter (process_pkg info) info.pkgs

end

let add_include dir =
  let files = Sys.readdir dir in
  let dir = if dir = "." then [] else Paths.S.parse_filename dir in
  let includes =
    Array.fold_left (fun m x ->
        let polycy =
          let open Fault in
          Polycy.set_err (Self_polycy.unknown_extension, Level.whisper)
            !param.polycy in
        match classify polycy !param.synonyms x with
        | None | Some { kind = Signature; _ } -> m
        | Some { kind = Interface | Implementation ; _ } ->
          Name.Map.add (Read.name x)
            Pkg.( dir / local x) m
      )
      !param.includes files
  in
  param :=
    { !param with includes }

let no_implicits () =
  param := { !param with implicits = false }

let no_include () =
  param := { !param with no_include = true }



let fault s =
  match String.split_on_char '=' s with
  | [] | [_]| _ :: _ :: _ :: _ -> ()
  | [a;b] ->
    let path= List.map String.trim @@ String.split_on_char '.' a in
    let level = Fault.Level.of_string b in
    let polycy = (!param).polycy in
    param := { !param with polycy = Fault.Polycy.set (path,None,level) polycy }

let silent_level s =
  let polycy = (!param).polycy in
  param := { !param with polycy = { polycy with silent = Fault.Level.of_string s} }

let exit_level s =
  let polycy = (!param).polycy in
  param := { !param with polycy = { polycy with exit = Fault.Level.of_string s} }

let print_polycy ()=
  Fault.Polycy.pp Pp.std (!param).polycy

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

let args = Cmd.[
    "-absname", Cmd.Unit abs_path, ": use absolute path name";
    "-all", Unit all, ": display full dependencies in makefile";
    "-allow-approx", Unit allow_approx,": fall back to approximated parser \
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
    "-modules", Unit (set @@ modules ~filter:inner_filter),
    ": print raw module dependencies";
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


    "-aliases", Unit (set pp_aliases), ": print aliases";
    "-info", Unit (set info), ": print detailed information";
    "-export", Unit (set export), ": export resolved modules signature";

    "-dot", Unit (set dot), ": print dependencies in dot format";
    "-dsort", Unit(set dsort),": print unit paths in topological order";
    "-makefile", Unit (set makefile), ": print makefile depend file(default)";
    "-approx-m2l", Unit (set_iter approx_file), ": print approximated m2l ast";
    "-m2l", Unit (set_iter m2l), ": print m2l ast";
    "-m2l-sexp", Unit (set_iter m2l_sexp), ": print m2l ast in s-expression format";

    "-one-pass", Unit (set_iter one_pass), ": print m2l ast after one pass";
    "-sig", Unit (set sign), ": print inferred signature";
    "-sig-only", Unit sig_only,
    ": filter produced m2l to keep only signature-level elements.\
     \n\n Module suboptions:\n";

    "-nl-modules", Unit (set @@ line_modules ~filter:dep_filter),
    ": print new-line separated raw dependencies";
    "-extern-modules", Unit (set @@ modules ~filter:lib_filter),
    ": print raw extern dependencies";
    "-inner-modules", Unit (set @@ modules ~filter:inner_filter),
    ": print raw inner dependencies";
    "-unknown-modules", Unit (set @@ modules ~filter:extern_filter),
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
    "-native-filter", Cmd.Unit native,
    ": generate native compilation only dependencies";
    "-bytecode-filter", Cmd.Unit bytecode,
    ": generate bytecode only dependencies.\n\n Fault options:\n";

    "-closed-world", Unit close_world,
    ": require that all dependencies are provided";
    "-k", Unit keep_going, ": ignore most recoverable errors and keep going";
    "-strict", Unit strict, ": fail rather than approximate anything";
    "-quiet", Unit quiet,
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
    "-no-alias-deps", Cmd.Unit (fun () -> transparent_aliases true),
    ": delay aliases dependencies";
    "-o", Cmd.String o, "<filename>: set current output file";
    "-no-implicits", Cmd.Unit no_implicits,
    ": do not implicitly search for a mli \
     file when given a ml file input";
    "-no-include", Cmd.Unit no_include, ": do not include base directory by default";
    "-no-stdlib", Cmd.Unit no_stdlib,
    ": do not use precomputed stdlib environment";
    "-read-sig", Cmd.String read_sig,
    "<signature>: add signature to the base environment";
    "-see", Cmd.String add_invisible_file,
    "<file>: use <file> in dependencies computation but do not display it.";
    "-transparent-extension-node", Cmd.Bool transparent_extension,
    "<bool>: inspect unknown extension nodes\n"
  ]

let () =
  Compenv.readenv stderr Before_args
  ; if not !param.no_include then add_include "."
  ; Cmd.parse args add_file usage_msg
  ; Findlib.process ()
  ; Compenv.readenv stderr Before_link
  ; if !action = [] then set makefile ()
  ; List.iter act !action
