
module Pkg = Paths.Pkg
module Pth = Paths.Simple

type t = {
  name: string;
  path: Pkg.t;
  kind: M2l.kind;
  code: M2l.t;
  dependencies: Pkg.set
}
type unit = t
type 'a pair = { ml:'a; mli:'a}


module Group = struct
  type t = unit option pair
  type group = t

exception Collision of { previous:unit; collision:unit}
let add_mli mli x =
  match x.mli with
  | Some previous when mli = previous -> x
  | Some previous -> raise @@ Collision {previous; collision=mli}
  | None -> { x with mli = Some mli }

let add_ml ml x =
  match x.ml with
  | None -> { x with ml = Some ml }
  | Some previous when ml = previous -> x
  | Some previous -> raise @@ Collision {previous; collision=ml}

let add unit x = match unit.kind with
  | Structure -> add_ml unit x
  | Signature -> add_mli unit x

let empty = { mli = None; ml = None }

module Map = struct
  type t = group Paths.Simple.Map.t

let add unit m =
  let key = Paths.S.chop_extension unit.path.file in
  let grp = Option.( Pth.Map.find_opt key m >< empty ) in
  Pth.Map.add key (add unit grp) m

let of_list = List.fold_left (fun x y -> add y x) Pth.Map.empty
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

let read_file kind filename =
  let name, code = Read.file kind filename in
  { name;
    kind;
    path = Pkg.local filename;
    code;
    dependencies = Pkg.Set.empty
  }


let group_by classifier files =
  let start = Pth.Map.empty in
  List.fold_left (fun m f ->
      let kind =  classifier f in
      let unit = read_file kind f in
      Group.Map.add unit m) start files

let group {ml;mli} =
  let start = Pth.Map.empty in
  let add kind m f =
    let unit = read_file kind f in
    Group.Map.add unit m in
  let mid = List.fold_left (add Structure) start ml in
  List.fold_left (add Signature) mid mli


let split map =
  List.fold_left ( fun {ml; mli} (_name,grp) ->
      match grp.ml, grp.mli with
      | Some ml', Some mli' -> { ml = ml' :: ml; mli = mli' :: mli }
      | None, None -> { ml; mli }
      | Some m, None | None, Some m -> { ml; mli = m :: mli }
    ) { ml = []; mli = [] }  (Pth.Map.bindings map)


module Set = Set.Make(struct type t = unit let compare = compare end)
