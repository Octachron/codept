module Cmd = Arg
open M2l

module S = Module.Sig
let std = Format.std_formatter

module Param = struct
  let transparent_extension_nodes = true
  let transparent_aliases = true
end

let classify f =
  if Filename.check_suffix f ".mli" then
    Unit.Signature
  else
    Unit.Structure

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
  let module Sg = Compute.Sg(Param) in
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

(*
let files = match Array.to_list Sys.argv with
  | [] -> assert false
  |  _ :: q -> q
*)

(*
  let order =
    let compute (i,m) (u:Unit.t) = i+1, Name.Map.add u.name i m in
    snd @@ List.fold_left compute (0,Name.Map.empty) @@ List.rev_map units

  let compare order x y =
    let get x=Name.Map.find_opt x order in
    match get x, get y with
    | Some k , Some l -> compare k l
    | None, Some _ -> -1
    | Some _, None -> 1
    | None, None -> compare x y
*)


(*  let files = match Array.to_list Sys.argv with
    | [] -> assert false
    |  _ :: q -> q
*)



  (*
  let order =
    let compute (i,m) u = i+1, Name.Map.add u.Unit.name i m in
    snd @@ List.fold_left compute (0,Name.Map.empty) @@ List.rev units

  let topos_compare order x y =
    let get x=Name.Map.find_opt x order in
    match get x, get y with
    | Some k , Some l -> compare k l
    | None, Some _ -> -1
    | Some _, None -> 1
    | None, None -> compare x y
*)



let deps files =
  let names = List.map Unit.extract_name files in
  let units = Unit.( split @@ group_by classify files ) in
  let () =
    List.iter (Unit.pp std) units.mli;
    List.iter (Unit.pp std) units.ml;
    Pp.p "***\n***\n***\n"
  in
  let module Envt = Compute.Tracing in
  let core = Envt.start @@ Name.Set.of_list names in
  let module Solver = Unit.Make(Param) in
  let {Unit.ml; mli} = Solver.resolve_split_dependencies core units in
  List.iter (Unit.pp std) mli;
  List.iter (Unit.pp std) ml

let pp_module ppf u =
  let open Unit in
  Pp.fp ppf "%a: %a\n" Pp.(tlist ~sep:"/" string) u.path.file
    Pp.(tlist ~sep:" " string)
    ((*List.sort (compare order) @@*) Name.Set.elements u.dependencies)

let modules files =
  let names = List.map Unit.extract_name files in
  let units = Unit.( split @@ group_by classify files ) in
  let module Envt = Compute.Tracing in
  let core = Envt.start @@ Name.Set.of_list names in
  let module Solver = Unit.Make(Param) in
  let {Unit.ml; mli} = Solver.resolve_split_dependencies core units in
  let print units = List.iter (pp_module std)
      (List.sort Unit.(fun x y -> compare x.path.file y.path.file) units) in
    print ml; print mli


let usage_msg = "codept is an alternative dependencies solver for OCaml"

(* let extract_files () =
  let current = 1 + !(Cmd.current) in
  let rec files_from k = if k = Array.length Sys.argv then []
    else Sys.argv.(k) :: files_from (k+1) in
  files_from current
*)


let files = ref []
let anon_fun name =  files:= name :: ! files

let action = ref deps
let set command () = action:= command
let set_iter command = set (List.iter command)

let args =
  Cmd.["-modules", Unit (set modules), "print raw modules dependencies";
       "-deps", Unit (set deps), "print detailed dependencies";
       "-m2l", Unit (set_iter m2l), "print m2l ast";
       "-one-pass", Unit (set_iter one_pass), "print m2l ast after one pass"
    ]

let () =
  Cmd.parse args anon_fun usage_msg
  ; !action !files
