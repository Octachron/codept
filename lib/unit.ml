
module Pkg = Package

type kind = Structure | Signature

type t = {
  name: string;
  path: Pkg.t;
  kind: kind;
  code: M2l.t;
  dependencies: Pkg.set
}
type unit = t

module Group = struct
  type t = { impl:unit option; intf:unit option }
  type group = t

exception Collision of { previous:unit; collision:unit}
let add_mli mli x =
  match x.intf with
  | Some previous when mli = previous -> x
  | Some previous -> raise @@ Collision {previous; collision=mli}
  | None -> { x with intf = Some mli }

let add_ml ml x =
  match x.impl with
  | None -> { x with impl = Some ml }
  | Some previous when ml = previous -> x
  | Some previous -> raise @@ Collision {previous; collision=ml}

let add unit x = match unit.kind with
  | Structure -> add_ml unit x
  | Signature -> add_mli unit x

let empty = { intf = None; impl = None }

module Map = struct
  type t = group Npath.Map.t

let add unit m =
  let key = Pkg.chop_suffix unit.path.file in
  let grp = Option.( Npath.Map.find_opt key m >< empty ) in
  Npath.Map.add key (add unit grp) m

let of_list = List.fold_left (fun x y -> add y x) Npath.Map.empty
end

end



let pp ppf unit =
  Pp.fp ppf "@[<hov2>[ name=%s; @, path=%a; @;\
             m2l = @[%a@]; @;\
             dependencies=@[%a@] @;\
             ] @] @."
    unit.name
    Pkg.pp_simple unit.path
    M2l.pp unit.code
    Pkg.Set.pp unit.dependencies




let (@<) f g x = f @@ g x
let (%>) f g x = x |> f |> g

let extract_name filename = String.capitalize_ascii @@
  Filename.chop_extension @@ Filename.basename filename


let read_file kind filename =
  let name = extract_name filename in
  Location.input_name := filename;
  let input_file = Pparse.preprocess filename in
  let code =  try
      match kind with
      | Structure ->
        Ast_analyzer.structure @@
        Pparse.file Format.err_formatter ~tool_name:"codept" input_file
          Parse.implementation Pparse.Structure
      | Signature ->
        Ast_analyzer.signature @@
        Pparse.file Format.err_formatter ~tool_name:"codept" input_file
          Parse.interface Pparse.Signature
    with Syntaxerr.Error _ ->
      Pparse.remove_preprocessed input_file;
      Error.log "Syntax error in %s\n" filename
  in
  Pparse.remove_preprocessed input_file;
  { name;
    kind;
    path = { Pkg.source = Local; file=Pkg.parse_filename filename };
    code;
    dependencies = Pkg.Set.empty
  }

type 'a split = { ml:'a; mli:'a}

let group_by classifier files =
  let start = Npath.Map.empty in
  List.fold_left (fun m f ->
      let kind =  classifier f in
      let unit = read_file kind f in
      Group.Map.add unit m) start files

let group {ml;mli} =
  let start = Npath.Map.empty in
  let add kind m f =
    let unit = read_file kind f in
    Group.Map.add unit m in
  let mid = List.fold_left (add Structure) start ml in
  List.fold_left (add Signature) mid mli


let split map =
  List.fold_left ( fun {ml; mli} (_name,grp) ->
      match Group.(grp.impl, grp.intf) with
      | Some ml', Some mli' -> { ml = ml' :: ml; mli = mli' :: mli }
      | None, None -> { ml; mli }
      | Some m, None | None, Some m -> { ml; mli = m :: mli }
    ) { ml = []; mli = [] }  (Npath.Map.bindings map)


module Set = Set.Make(struct type t = unit let compare = compare end)
