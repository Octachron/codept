
module Pth = Paths.Simple

type precision =
  | Exact
  | Approx

(** Base type *)
type 'ext t = {
  path: Namespaced.t; (** module path of the compilation unit *)
  src: Pkg.t; (** source file of the compilation unit *)
  kind: M2l.kind;
  precision: precision;
  code: M2l.t;
  more: 'ext
}

(** Extension for output type *)
type ext = {
  signature: Module.signature;
  dependencies: Deps.t
}

type 'ext base = 'ext t
type s = unit t
type u = ext t
type r = u

let signature unit = unit.more.signature
let deps u = u.more.dependencies
let update dependencies u =
  { u with more = { u.more with dependencies } }

let local_dependencies unit =
  let filter { Deps.pkg; _ } =
    match pkg with
    | {Pkg.source=(Unknown | Special _); _ } -> false
    | _ -> true
  in
  List.filter filter @@ Deps.all @@ deps unit

let lift signature dependencies u =
  {u with more = {signature;dependencies} }

let proj u = { u with more = () }


let read_file policy kind filename path : s =
  let code = Read.file_raw kind filename in
  let precision, code = match code with
    | Ok c -> Exact, c
    | Error (Serialized e) ->
      Standard_faults.schematic_errors policy (filename,"m2l",e);
      Approx, []
    | Error (Ocaml (Syntax msg)) ->
      Fault.raise policy Standard_faults.syntaxerr msg;
      Approx, Approx_parser.lower_bound filename
    | Error (Ocaml (Lexer msg)) ->
      Fault.raise policy Standard_faults.lexerr (!Location.input_name,msg);
      Approx, Approx_parser.lower_bound filename

  in
      { path;
        kind = kind.kind;
        precision;
        src = Pkg.local filename;
        code;
        more = ()
      }

type 'a pair = { ml:'a; mli:'a}
let map fs xs = { ml = fs.ml xs.ml; mli = fs.mli xs.mli}
let unimap f xs = { ml = f xs.ml; mli = f xs.mli }

let adder add p = function
  | M2l.Structure, x -> { p with ml = add x p.ml }
  | M2l.Signature, x -> { p with mli = add x p.mli }

module Group = struct

  let key unit =  unit.path

  type 'ext group = 'ext t list pair

  let add_mli mli x =
    { x with mli = mli :: x.mli }

  let add_ml ml x =
    { x with ml =  ml :: x.ml }

  let raw_add kind elt x =
    match kind with
    | M2l.Structure -> add_ml elt x
    | Signature -> add_mli elt x
  let add elt x = raw_add elt.kind elt x

  let empty = { mli = []; ml = [] }

  module Map = struct
    type 'ext t = 'ext group Namespaced.Map.t

    let find path m = Namespaced.Map.find path m

    let raw_add kind unit m =
      let key = key unit in
      let grp = Option.default empty (Namespaced.Map.find_opt key m) in
      Namespaced.Map.add key (raw_add kind unit grp) m

    let add unit m = raw_add unit.kind unit m


    let of_list x = List.fold_left (fun x y -> add  y x) Namespaced.Map.empty x

    let fold f map start = Namespaced.Map.fold (fun _ -> f) map start
    let iter f map = fold (fun x () -> f x) map ()
  end

  let group {ml;mli} =
    let start = Namespaced.Map.empty in
    let add kind m x = Map.raw_add kind x m in
    let mid = List.fold_left (add Structure) start ml in
    List.fold_left (add Signature) mid mli

  let flatten grp =
    let flat  = function
      | [] -> None, []
      | [x] -> Some x, []
      | x :: q -> Some x, x::q in
    let mli, mli_err = flat grp.mli in
    let ml, ml_err = flat grp.ml in
    { ml; mli }, { ml = ml_err; mli = mli_err }

  let split map =
    Namespaced.Map.fold ( fun name grp ({ml; mli}, errors ) ->
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
      ) map ({ ml = []; mli = [] },[])

end

let pp ppf unit =
  Pp.fp ppf "@[<2>[@ path=%a;@ source=%a;@ \
             m2l = @[%a@];@ \
             signature=[ @[%a@] ];@ \
             dependencies=@[%a@]@ \
             ]@]@."
    Namespaced.pp unit.path
    Pkg.pp_simple unit.src
    M2l.pp unit.code
    Module.pp_signature (signature unit)
    Deps.pp (deps unit)

let pp_input ppf (unit:s) =
  Pp.fp ppf "@[<hov2>[ path=%a;@ source=%a;@ \
             m2l = @[%a@];@ \
             ] @]@."
    Namespaced.pp unit.path
    Pkg.pp_simple unit.src
    M2l.pp unit.code

module Set = Set.Make(struct type t = u let compare = compare end)
