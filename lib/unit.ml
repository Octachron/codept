
module Map = Name.Map


type package = {name:string; content: content }
and content = Subpackages of package Map.t | Modules of Module.t Map.t

type package_path = Local | Sys of Npath.t

type path = { package: package_path ; file: Npath.t }

let rec chop_suffix l = match l with
  | [] -> []
  | [a] -> [Filename.chop_extension a]
  | a :: q -> a :: chop_suffix q

let pp_package_path ppf = function
  | Local -> Pp.fp ppf "Local"
  | Sys n -> Pp.fp ppf "Sys[%a]" Npath.pp n

let pp_path ppf {package;file}=
  Pp.fp ppf "(%a)%a" pp_package_path package Npath.pp file

type kind = Structure | Signature

type t = {
  name: string;
  path: path;
  kind: kind;
  code: M2l.t;
  dependencies: Name.set
}
type unit = t

module Group = struct
  type t = { impl:unit option; intf:unit option }
  type group = t

exception Collision of { previous:unit; collision:unit}
let add_mli mli x =
  match x.intf with
  | Some previous -> raise @@ Collision {previous; collision=mli}
  | None -> { x with intf = Some mli }

let add_ml ml x =
  match x.impl with
  | Some previous -> raise @@ Collision {previous; collision=ml}
  | None -> { x with impl = Some ml }

let add unit x = match unit.kind with
  | Structure -> add_ml unit x
  | Signature -> add_mli unit x

let empty = { intf = None; impl = None }

module Map = struct
  type t = group Npath.Map.t

let add unit m =
  let key = chop_suffix unit.path.file in
  let grp = Option.( Npath.Map.find_opt key m >< empty ) in
  Npath.Map.add key (add unit grp) m
end

end



let pp ppf unit =
  Pp.fp ppf "@[<hov2>[ name=%s; @, path=%a; @;\
             m2l = @[%a@]; @;\
             dependencies=@[%a@] @;\
             ] @] @."
    unit.name
    pp_path unit.path
    M2l.pp unit.code
    Name.Set.pp unit.dependencies




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
    kind;
    path = { package= Local; file=[filename] };
    code;
    dependencies = Name.Set.empty
  }

let group_by classifier files =
  let start = Npath.Map.empty in
  List.fold_left (fun m f ->
      let kind =  classifier f in
      let unit = read_file kind f in
      Group.Map.add unit m) start files

type 'a split = { ml:'a; mli:'a}

let split map =
  List.fold_left ( fun {ml; mli} (_name,grp) ->
      match Group.(grp.impl, grp.intf) with
      | Some ml', Some mli' -> { ml = ml' :: ml; mli = mli' :: mli }
      | None, None -> { ml; mli }
      | Some m, None | None, Some m -> { ml; mli = m :: mli }
    ) { ml = []; mli = [] }  (Npath.Map.bindings map)

module Eval = Compute.Tr
module Envt = Compute.Tracing

let compute_more core unit =
  let env = Envt.create core in
  let result = Eval.m2l env unit.code in
  !(env.deps), result

exception Cycle

let eval ?(learn=true) (finished, core, rest) unit =
  let open M2l in
  match compute_more core unit with
  | deps, Work.Done (_,sg) ->
    let core =
      if learn then begin
        let md = Module.(create ~origin:Unit) unit.name sg in
        Envt.add_core core md
      end
      else
        core
    in
    let deps = Name.Set.union unit.dependencies deps in
    let unit = { unit with code = [Defs (Definition.sg_bind sg)];
                           dependencies = deps } in
    (unit :: finished, core, rest )
  | deps, Halted code ->
    let deps = Name.Set.union unit.dependencies deps in
    let unit = { unit with dependencies = deps; code } in
    finished, core, unit :: rest


let resolve_dependencies ?(learn=true) core units =
  let rec resolve alert env solved units =
    let solved, env, units' =
      List.fold_left (eval ~learn) (solved,env,[]) units in
    match List.length units' with
    | 0 -> env, solved
    | n when n = List.length units ->
      if alert then
        ( List.iter (pp Pp.err) units;
          raise Cycle
        )
      else resolve true env solved units'
    | _ ->
      resolve false env solved units' in
  resolve false core [] units

let resolve_split_dependencies env {ml; mli} =
  let env, mli = resolve_dependencies env mli in
  let _, ml = resolve_dependencies ~learn:false env ml in
  { ml; mli }
