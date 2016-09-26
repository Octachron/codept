type spath = string list

module Map = Name.Map

type package = {name:string; content: content }
and content = Subpackages of package Map.t | Modules of Module.t Map.t

type package_path = Local | Sys of spath

type path = { package: package_path ; file: spath }

type t = {
  name: string;
  path: path;
  signature: Module.explicit_signature;
  unresolved: Unresolved.direct_map;
  dependencies: Name.set
}

let pp ppf unit =
  Pp.fp ppf "@[[ name=%s; @, path=…; @;\
             signature=@[[%a]@]; @;\
             unresolved=@[[%a]@]; @;\
             dependencies=@[%a@] @;\
             ] @] @."
    unit.name
    Module.pp_explicit unit.signature
    Unresolved.pp (Unresolved.Map unit.unresolved)
    Name.Set.pp unit.dependencies

type kind = Structure | Signature


let (@<) f g x = f @@ g x
let (%>) f g x = x |> f |> g

let extract_name filename = String.capitalize_ascii @@
  Filename.chop_extension @@ Filename.basename filename

let read_file env kind filename =
  let name =  extract_name filename in
  let extract env = Envt.(env.unresolved, env.signature) in
  let normalize (focus, sign) =
    match sign with
    | Module.Sig esn ->
      ( focus, esn )
    | Module.Alias _ | Module.Fun _ -> assert false
  in
  let parse_and_analyze = match kind with
    | Structure -> Parse.implementation %> Ast_analyzer.structure env %> extract
    | Signature -> Parse.interface %> Ast_analyzer.signature env %> normalize in
  let foc, signature = try parse_and_analyze @@
      Lexing.from_channel @@ open_in filename
    with
    | Error.Opening_a_functor msg ->
      Error.log "Analyse error when reading %s@: opening functor %s." filename msg
    | Error.Functor_expected msg -> Error.log
      "Analyse error when reading %s@: functor expected, got %s." filename msg


  in
  { name;
    path = { package= Local; file=[filename] };
    signature;
    unresolved = Unresolved.map foc;
    dependencies = Name.Set.empty
  }

let refine (type env) ((module Refine):env Refiner.t) env unit =
  let deps = Name.Set.empty in
  try
  let deps, Unresolved.Map map =
    Refine.map deps env (Unresolved.Map unit.unresolved) in
  { unit with
    unresolved = map;
    signature = Refine.explicit_signature env unit.signature;
    dependencies = (Name.Set.union deps unit.dependencies)
  } with
  | Error.Opening_a_functor msg ->
    Error.log "Refinement error for module %s: opening functor %s." unit.name msg
  | Error.Functor_expected msg -> Error.log
      "Analyse error when reading %s: functor expected, got %s." unit.name msg


let is_independent unit = Unresolved.is_empty unit.unresolved

exception Cycle

let resolve_dependencies (type env) (refiner: env Refiner.t) env units =
  let (module R) = refiner in
  let remove_independents (env, l, solved) unit =
    if is_independent unit then
      let env = R.add_root env unit.name unit.signature in
      env, l, unit :: solved
    else
      env, unit :: l, solved in
  let rec resolve alert env solved units =
    let env, units', solved =
      List.fold_left remove_independents (env,[],solved) units in
    match List.length units' with
    | 0 -> solved
    | n when n = List.length units ->
      if alert then
        ( List.iter (pp Pp.err) units;
          raise Cycle
        )
      else resolve true env solved @@ List.map (refine refiner env) units
    | _ ->
      let units = List.map (refine refiner env) units' in
      resolve false env solved units in
  resolve false env [] units
