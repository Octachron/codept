open Unit

let test = {|
 open I
 module U = struct module I = struct module E = Ext  end end
 open A
 module K = struct open W.S module X = Y end
 let x = B.x
 module F(X:sig end) = struct module N = Xeno end
 open R
 open C
 open U.I
 open E
 open K.X

|}


let std = Format.std_formatter

module File() = struct
  let lex_test = Lexing.from_channel @@ open_in Sys.argv.(1)

  let ast = Parse.implementation lex_test

  let () =
    Ast_analyzer.structure Envt.empty ast
    |> Envt.pp std
end

module Refine() = struct

  let parse st =
    let lex = Lexing.from_string st in
    let ast = Parse.implementation lex in
    Ast_analyzer.structure Envt.empty ast


  let a = {|
    open B
    open C
    module type S = sig end
|}

let b = {| module B = struct module C = struct end end |}

let env_a = parse a
let env_b = parse b
let env_for_a = Envt.(Resolver.add_root empty "A"
    env_b.signature)
let deps, u =
    Refiner.Envt.map Name.Set.empty env_for_a env_a.Envt.unresolved.Unresolved.map

let () = Format.printf
    "env A:@;@[<hov2>%a@]\n env B:@;@[%a@]\n env A | env B:%a;%a\n@."
    Envt.pp env_a
    Envt.pp env_b
    Unresolved.pp u
    Pp.(clist string) (Name.Set.elements deps)

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

  let env0 = Envt.empty

  let units = List.map (fun f->
      Unit.read_file env0 (classify f) f) files


  let () = List.iter (Unit.pp std) units;
    Pp.p "\n----------------------------------------------------------\n"


  let env = Name.Set.of_list names, env0

  let refiner = Refiner.( precise_filter ++ envt)

  let units = resolve_dependencies refiner env units

  let order =
    let compute (i,m) u = i+1, Name.Map.add u.name i m in
    snd @@ List.fold_left compute (0,Name.Map.empty) @@ List.rev units

  let compare order x y =
    let get x=Name.Map.find_opt x order in
    match get x, get y with
    | Some k , Some l -> compare k l
    | None, Some _ -> -1
    | Some _, None -> 1
    | None, None -> compare x y

  let () = List.iter (Unit.pp ~compare:(compare order) std) units

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
  let env0 = Envt.empty

  let units = List.map (fun f->
      Unit.read_file env0 (classify f) f) files

  let env = Name.Set.of_list names, env0
  let refiner = Refiner.( precise_filter ++ envt)

  let units = resolve_dependencies refiner env units

  let order =
    let compute (i,m) u = i+1, Name.Map.add u.name i m in
    snd @@ List.fold_left compute (0,Name.Map.empty) @@ List.rev units

  let compare order x y =
    let get x=Name.Map.find_opt x order in
    match get x, get y with
    | Some k , Some l -> compare k l
    | None, Some _ -> -1
    | Some _, None -> 1
    | None, None -> compare x y

  let pp_module ppf u =
    let open Unit in
    Pp.fp ppf "%a:%a\n" Pp.(list ~sep:"/" string) u.path.file
      Pp.(list ~sep:" " string)
      (List.sort (compare order) @@ Name.Set.elements u.dependencies)

  let () = List.iter (pp_module std) units

end

module Do = Deps()
