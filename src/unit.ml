
module Map = Name.Map


type package = {name:string; content: content }
and content = Subpackages of package Map.t | Modules of Module.t Map.t

type package_path = Local | Sys of Npath.t

type path = { package: package_path ; file: Npath.t }

type t = {
  name: string;
  path: path;
  code: M2l.t;
  dependencies: Name.set
}

let pp ppf unit =
  Pp.fp ppf "@[<hov2>[ name=%s; @, path=…; @;\
             m2l = @[%a@]; @;\
             dependencies=@[%a@] @;\
             ] @] @."
    unit.name
    M2l.pp unit.code
    Name.Set.pp unit.dependencies

type kind = Structure | Signature


let (@<) f g x = f @@ g x
let (%>) f g x = x |> f |> g

let extract_name filename = String.capitalize_ascii @@
  Filename.chop_extension @@ Filename.basename filename

let read_file kind filename =
  let name = extract_name filename in
  let parse = match kind with
    | Structure -> Parse.implementation %> Ast_analyzer.structure
    | Signature -> Parse.interface %> Ast_analyzer.signature in
  let code = parse @@ Lexing.from_channel @@ open_in filename
  in
  { name;
    path = { package= Local; file=[filename] };
    code;
    dependencies = Name.Set.empty
  }

module Eval = Compute.Tr
module Envt = Compute.Tracing

let compute_more core unit =
  let env = Envt.create core in
  let result = Eval.m2l env unit.code in
  !(env.deps), result

exception Cycle

let eval (finished, core, rest) unit =
  let open M2l in
  match compute_more core unit with
  | deps, Work.Done (_,sg) ->
    let md = Module.create ~origin:Module.Unit unit.name sg in
    let core = Envt.add_core core md in
    let deps = Name.Set.union unit.dependencies deps in
    let unit = { unit with code = []; dependencies = deps } in
    (unit :: finished, core, rest )
  | deps, Halted code ->
    let deps = Name.Set.union unit.dependencies deps in
    let unit = { unit with dependencies = deps; code } in
    finished, core, unit :: rest


let resolve_dependencies core units =
  let rec resolve alert env solved units =
    let solved, env, units' =
      List.fold_left eval (solved,env,[]) units in
    match List.length units' with
    | 0 -> solved
    | n when n = List.length units ->
      if alert then
        ( List.iter (pp Pp.err) units;
          raise Cycle
        )
      else resolve true env solved units'
    | _ ->
      resolve false env solved units' in
  resolve false core [] units
