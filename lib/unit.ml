
module Pkg = Paths.Pkg
module Pth = Paths.Simple

type precision =
  | Exact
  | Approx

type s = {
  path: Namespaced.t;
  src: Pkg.t;
  kind: M2l.kind;
  precision: precision;
  code: M2l.t;
}

type r = {
  path: Namespaced.t;
  src: Pkg.t;
  kind: M2l.kind;
  precision: precision;
  code: M2l.t;
  signature: Module.signature;
  dependencies: Deps.t
}
type u = r

let lift signature dependencies ({src;path;kind;precision;code}:s) =
  {signature;dependencies;src;path;kind;precision;code}

let proj {src;path;kind;precision;code; _ }: s=
  {src;path;kind;precision;code}


let read_file policy kind filename path : s =
  let _name, code = Read.file kind filename in
  let precision, code = match code with
    | Ok c -> Exact, c
    | Error (Serialized e) ->
      Standard_faults.schematic_errors policy (filename,"m2l",e);
      Approx, []
    | Error (Ocaml (Syntax msg)) ->
      Fault.handle policy Standard_faults.syntaxerr msg;
      Approx, Approx_parser.lower_bound filename
    | Error (Ocaml (Lexer msg)) ->
      Fault.handle policy Standard_faults.lexerr !Location.input_name msg;
      Approx, Approx_parser.lower_bound filename

  in
      { path;
        kind = kind.kind;
        precision;
        src = Pkg.local filename;
        code
      }

type 'a pair = { ml:'a; mli:'a}
let map fs xs = { ml = fs.ml xs.ml; mli = fs.mli xs.mli}
let unimap f xs = { ml = f xs.ml; mli = f xs.mli }

let adder add p = function
  | M2l.Structure, x -> { p with ml = add x p.ml }
  | M2l.Signature, x -> { p with mli = add x p.mli }


module type group =
sig
  type elt
  type set
  type ('a,'b) arrow
  type t = set pair
  type group = t

  val add_mli : elt -> group -> group
  val add_ml : elt -> group -> group
  val add : (M2l.kind, elt -> group -> group) arrow
  val empty : group
  module Map :
  sig
    type t = group Pth.map
    val find: Pth.t -> t -> group
    val add : (M2l.kind , elt -> t -> t) arrow
    val of_list : (M2l.kind, elt list -> t) arrow
  end

  val group : elt list pair -> group Pth.map
  val flatten: group -> elt option pair * elt list pair
  val split : group Pth.map -> elt list pair * (Paths.S.t * elt list) list

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
    module S = Set.Make(struct type t = elt let compare = compare end)
    type set = S.t
    type t = set pair
    type group = t

    let add_mli mli x =
      { x with mli = S.add mli x.mli }

    let add_ml ml x =
      { x with ml = S.add ml x.ml }

    let raw_add extr elt x =
      match extr elt with
      | M2l.Structure -> add_ml elt x
      | Signature -> add_mli elt x

    let add = lift raw_add

    let empty = { mli = S.empty; ml = S.empty }

    module Map = struct
      type t = group Paths.Simple.Map.t

      let find path m = Paths.Simple.Map.find (Paths.S.chop_extension path) m

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

      let flatten grp =
        let flat s =
          let n = S.cardinal s in
          if n = 0 then
            None, []
          else if n = 1 then
            Some (S.choose s), []
          else
            Some (S.choose s), S.elements s in
        let mli, mli_err = flat grp.mli in
        let ml, ml_err = flat grp.ml in
        { ml; mli }, { ml = ml_err; mli = mli_err }

      let split (map: Map.t) =
        List.fold_left ( fun ({ml; mli}, errors ) (name,grp) ->
            let g, err = flatten grp in
            let err = err.ml @ err.mli in
            let errors = if err = [] then errors else
                (name, err) :: errors in
            begin match g with
              | { ml = Some x; mli = None }
              | { ml = None; mli = Some x } ->
                { ml; mli = x :: mli }
              | { ml = Some x ;mli = Some y} ->
                { ml = x::ml; mli = y::mli}
              | { ml = None; mli = None } -> {ml;mli}
            end
            ,  errors
          ) ({ ml = []; mli = [] },[])  (Pth.Map.bindings map)

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
        let key (unit:elt) = Paths.S.chop_extension unit.src.file
      end)

    module R = Make(struct
        type elt = r
        type ('a,'b) arrow = 'b
        let lift f = f (fun (u:elt) -> u.kind)
        let key (unit:elt) = Paths.S.chop_extension unit.src.file
      end)

end



let pp ppf unit =
  Pp.fp ppf "@[<hov2>[ path=%a; @, source=%a; @;\
             m2l = @[%a@]; @;\
             signature=[ @[%a@] ];\
             dependencies=@[%a@] @;\
             ] @] @."
    Namespaced.pp unit.path
    Pkg.pp_simple unit.src
    M2l.pp unit.code
    Module.pp_signature unit.signature
    Deps.pp unit.dependencies

let pp_input ppf (unit:s) =
  Pp.fp ppf "@[<hov2>[ path=%a; @, source=%a; @;\
             m2l = @[%a@]; @;\
             ] @] @."
    Namespaced.pp unit.path
    Pkg.pp_simple unit.src
    M2l.pp unit.code

module Set = Set.Make(struct type t = u let compare = compare end)
