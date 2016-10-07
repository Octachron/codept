module Cmd = Arg
open M2l

module S = Module.Sig
let std = Format.std_formatter


let classify f =
  if Filename.check_suffix f ".mli" then
    Unit.Signature
  else
    Unit.Structure

let file f =
  let lex_test = Lexing.from_channel @@ open_in f in
  let ast = Parse.implementation lex_test in
  let start =  Ast_analyzer.structure ast in
  Pp.fp std "M2l: %a@." M2l.pp start;
  start
  |> Compute.basic
  |> Normalize.all
  |> snd
  |> Pp.fp std  "Basic:\n %a@." M2l.pp;
  match start |> Compute.Sg.m2l S.empty with
  | Done (_state,d) -> Pp.fp std "Computation finished:\n %a@." S.pp d
  | Halted h -> Pp.fp std "Computation halted at:\n %a@." M2l.pp h


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
  let {Unit.ml; mli} = Unit.resolve_split_dependencies core units in
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
  let {Unit.ml; mli} = Unit.resolve_split_dependencies core units in
  let print units = List.iter (pp_module std)
      (List.sort Unit.(fun x y -> compare x.path.file y.path.file) units) in
    print mli; print ml


let usage_msg = "fdep is an alternative dependencies solver for OCaml"

let extract_files () =
  let current = 1 + !(Cmd.current) in
  let rec files_from k = if k = Array.length Sys.argv then []
    else Sys.argv.(k) :: files_from (k+1) in
  files_from current


let cmd_module () =
  modules @@ extract_files ()

let cmd_deps () =
  deps @@ extract_files ()

let cmd_file () =
  file Sys.argv.(1 + !Cmd.current)

let anon_fun _name = ()

let args =
    Cmd.["-modules", Unit cmd_module, "print raw modules dependencies";
     "-deps", Unit cmd_deps, "print detailed dependencies";
     "-file", Unit cmd_file, "print simple analysis of one file"
    ]

let () = Cmd.parse args anon_fun usage_msg
