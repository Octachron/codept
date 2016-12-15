
module Pkg = Paths.Pkg
module Pth = Paths.Simple

type precision =
  | Exact
  | Approx

type s = {
  name: string;
  path: Pkg.t;
  kind: M2l.kind;
  precision: precision;
  code: M2l.t;
}

type r = {
  name: string;
  path: Pkg.t;
  kind: M2l.kind;
  precision: precision;
  code: M2l.t;
  signature: Module.signature;
  dependencies: Pkg.set
}
type u = r

let lift signature dependencies ({name;path;kind;precision;code}:s) =
  {signature;dependencies; name;path;kind;precision;code}

let proj {name;path;kind;precision;code; _ }: s=
  {name;path;kind;precision;code}


let read_file polycy kind filename : s =
  let name, code = Read.file kind filename in
  let precision, code = match code with
    | Ok c -> Exact, c
    | Error M2l -> assert false
    | Error (Ocaml msg) ->
      Fault.(handle polycy syntaxerr msg);
      Approx, Approx_parser.lower_bound filename
  in
      { name;
        kind = kind.kind;
        precision;
        path = Pkg.local filename;
        code
      }

type 'a pair = { ml:'a; mli:'a}
let map fs xs = { ml = fs.ml xs.ml; mli = fs.mli xs.mli}
let unimap f xs = { ml = f xs.ml; mli = f xs.mli }


module type group =
sig
  type elt
  type ('a,'b) arrow
  exception Collision of { previous:elt; collision:elt}
  type t = elt option pair
  type group = t

  val add_mli : elt -> group -> group
  val add_ml : elt -> group -> group
  val add : (M2l.kind, elt -> group -> group) arrow
  val empty : group
  module Map :
  sig
    type t = group Pth.map
    val add : (M2l.kind , elt -> t -> t) arrow
    val of_list : (M2l.kind, elt list -> t) arrow
  end

  val group : elt list pair -> group Pth.map
  val split : group Pth.map -> elt list pair

end

module type group_core= sig
  type elt
  type ('a,'b) arrow
  val lift: ( (elt ->M2l.kind) -> 'c ) -> (M2l.kind, 'c) arrow
  val key: elt -> Pth.t
end

module Groups = struct
  module Make(Core:group_core)(*:
                                group with type elt = Core.elt and type ('a,'b) arrow = ('a,'b) Core.arrow*)
  =
  struct
    include Core
    type t = elt option pair
    type group = t

    exception Collision of { previous:elt; collision:elt}
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

    let raw_add extr elt x =
      match extr elt with
      | M2l.Structure -> add_ml elt x
      | Signature -> add_mli elt x

    let add = lift raw_add

    let empty = { mli = None; ml = None }

    module Map = struct
      type t = group Paths.Simple.Map.t

      let raw_add extr unit m =
        let key = key unit in
        let grp = Option.( Pth.Map.find_opt key m >< empty ) in
        Pth.Map.add key (raw_add extr unit grp) m

      let add = lift raw_add

      let raw_of_list extr = List.fold_left
          (fun x y -> raw_add extr y x) Pth.Map.empty

      let of_list = lift raw_of_list
    end

      let group {ml;mli} =
        let start = Pth.Map.empty in
        let add kind m elt =
          Map.raw_add (fun _ -> kind) elt m in
        let mid = List.fold_left (add Structure) start ml in
        List.fold_left (add Signature) mid mli


      let split map =
        List.fold_left ( fun {ml; mli} (_name,grp) ->
            match grp.ml, grp.mli with
            | Some ml', Some mli' -> { ml = ml' :: ml; mli = mli' :: mli }
            | None, None -> { ml; mli }
            | Some m, None | None, Some m -> { ml; mli = m :: mli }
          ) { ml = []; mli = [] }  (Pth.Map.bindings map)

    end

    module Filename = Make(struct
        type elt = string
        type ('a,'b) arrow = 'a -> 'b
        let lift f = fun kind -> f (fun _ -> kind)
        let key x = Paths.S.chop_extension @@ Pth.parse_filename x
      end)

    module Unit = Make(struct
        type elt = s
        type ('a,'b) arrow = 'b
        let lift f = f (fun (u:elt) -> u.kind)
        let key (unit:elt) = Paths.S.chop_extension unit.path.file
      end)

    module R = Make(struct
        type elt = r
        type ('a,'b) arrow = 'b
        let lift f = f (fun (u:elt) -> u.kind)
        let key (unit:elt) = Paths.S.chop_extension unit.path.file
      end)

end



let pp ppf unit =
  Pp.fp ppf "@[<hov2>[ name=%s; @, path=%a; @;\
             m2l = @[%a@]; @;\
             signature=@[%a@] ];\
             dependencies=@[%a@] @;\
             ] @] @."
    unit.name
    Pkg.pp_simple unit.path
    M2l.pp unit.code
    Module.pp_signature unit.signature
    Pkg.Set.pp unit.dependencies

let pp_input ppf (unit:s) =
  Pp.fp ppf "@[<hov2>[ name=%s; @, path=%a; @;\
             m2l = @[%a@]; @;\
             ] @] @."
    unit.name
    Pkg.pp_simple unit.path
    M2l.pp unit.code

module Set = Set.Make(struct type t = u let compare = compare end)
