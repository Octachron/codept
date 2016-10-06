open M2l

module S = Module.Sig
let std = Format.std_formatter

module File() = struct
  let lex_test = Lexing.from_channel @@ open_in Sys.argv.(1)

  let ast = Parse.implementation lex_test

  let () =
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

end

module Deps() = struct
  let classify f =
    if Filename.check_suffix f ".mli" then
      Unit.Signature
    else
      Unit.Structure

  let files = match Array.to_list Sys.argv with
    | [] -> assert false
    |  _ :: q -> q

  let names = List.map Unit.extract_name files

  let units = List.map (fun f->
      Unit.read_file (classify f) f) files


  let () = List.iter (Unit.pp std) units;
    Pp.p "***\n***\n***\n"

  module Envt = Compute.Tracing
  let core = Envt.start @@ Name.Set.of_list names
  let units = Unit.resolve_dependencies core units

  let order =
    let compute (i,m) (u:Unit.t) = i+1, Name.Map.add u.name i m in
    snd @@ List.fold_left compute (0,Name.Map.empty) @@ List.rev units

  let compare order x y =
    let get x=Name.Map.find_opt x order in
    match get x, get y with
    | Some k , Some l -> compare k l
    | None, Some _ -> -1
    | Some _, None -> 1
    | None, None -> compare x y

  let () = List.iter (Unit.pp std) units

end

module Modules() = struct
  let classify f =
    if Filename.check_suffix f ".mli" then
      Unit.Signature
    else
      Unit.Structure

  let files = match Array.to_list Sys.argv with
    | [] -> assert false
    |  _ :: q -> q

  let names = List.map Unit.extract_name files

  let units = List.map (fun f->
      Unit.read_file (classify f) f) files

  module Envt = Compute.Tracing
  let core = Envt.start @@ Name.Set.of_list names
  let units = Unit.resolve_dependencies core units


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

  let pp_module ppf u =
    let open Unit in
    Pp.fp ppf "%a: %a\n" Pp.(tlist ~sep:"/" string) u.path.file
      Pp.(tlist ~sep:" " string)
      ((*List.sort (compare order) @@*) Name.Set.elements u.dependencies)

  let () = List.iter (pp_module std)
      (List.sort Unit.(fun x y -> compare x.path.file y.path.file) units)

end


module R = Modules()
