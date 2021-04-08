module M=Module
module Y = Summary
module F = Standard_faults
module P = M.Partial

module Arg = M.Arg

module S = M.Sig
module T = Transforms

let fault param x = Fault.handle param.T.policy x
let raisef param f t = fault param (Fault.emit f t)

let debug fmt = Format.ifprintf Pp.err ("Debug:" ^^ fmt ^^"@.")


type path = T.answer
type query = path T.query_result

type state_diff = Y.t
type path_in_context =
  { loc: Fault.loc;
    edge:Deps.Edge.t option;
    level: Module.level;
    seed: Id.seed;
    ctx: state_diff;
    path: Paths.S.t;
    within: Module.signature option
  }

type module_like = P.t
type m2l = S.t


let signature x = match x.P.mty with
  | Sig s -> Some s.signature
  | _ -> (* FIXME: error message ? *) None

let pp ppf x =
  Format.fprintf ppf
    "@[<2>{@ path=%a;@ loc=%a;@ edge=%a;@ level=%a;@ ctx=@ (@[%a@]);@ }@]"
    Paths.S.pp x.path
    Fault.locc x.loc (Pp.opt Deps.Edge.pp) x.edge
    Module.pp_level x.level Summary.pp x.ctx

let pp_ml = P.pp

let path x = x.T.main

let empty = P.empty

let abstract seed = P.{name=None; mty=Abstract (Id.create seed)}
let apply param loc ~f ~x = T.apply_arg param.T.policy loc ~arg:x ~f
let unpacked =
  let mty = Module.Sig { signature=Blank; origin=First_class} in
  { empty with mty }

let fn ~f ~x =
  let arg = Option.fmap (Arg.map (fun x -> x.P.mty)) x in
  { P.name=f.P.name; mty=Module.Fun (arg, f.P.mty) }

let ext param (loc:Fault.loc) (name:string) =
  if not param.T.transparent_extension_nodes then
    raisef param F.extension_ignored (loc,name)
  else raisef param F.extension_traversed (loc,name)

let ident x = match x.T.kind with
  | T.Mty Sig m -> P.of_module x.name m
  | T.Namespace n -> P.pseudo_module x.name n
  | Mty r -> {P.name=Some x.name; mty=r}

let m2l_add expr defs = S.merge defs (Y.defined expr)
let m2l_init = S.empty

let str = P.simple

let included param loc lvl e = T.gen_include param.T.policy loc lvl e


let m_with dels mt = match mt.P.mty with
  | Module.Abstract _ | Fun _ -> mt
  | Sig s ->
    let signature = T.with_deletions dels s.signature in
    { mt with mty = Sig { s with signature } }

let bind name me = match name with
    | None -> Y.empty
    | Some name -> T.bind_summary Module name me
let bind_sig name m = match name with
  | Some name ->  T.bind_summary Module_type name m
  | None -> Y.empty

let bind_rec_add name expr y =
  Y.merge y @@ Option.either (fun name -> (T.bind_summary Module name expr)) Y.empty name
let bind_rec_init = Y.empty
let opened param ~loc m = T.open_ param.T.policy loc m
let empty_diff = Y.empty

let final x = x


module State(Env:Stage.envt) = struct
  type env = Env.t
  type state = { initial:env; diff:state_diff; current:env }
  let merge state diff =
    { state with current = Env.extend state.current diff;
                 diff = Y.merge state.diff diff }

  let bind_arg st arg =
    match arg with
    | { Arg.name= Some name; signature } ->
      let mt = P.extend signature.P.mty in
      merge st (Y.define [name, mt])
    | _ -> st

  let is_alias param state s =
    param.T.transparent_aliases && Env.is_exterior s state.current

  let restart state diff =
    { initial=state.initial;
      diff;
      current= Env.extend state.initial diff
    }

  let bind_alias state name p =
    match name with
    | None -> Y.empty
    | Some name ->
      let path = Namespaced.of_path @@ Env.expand_path p state.current in
      let m = Module.Alias { path; phantom = None } in
      Y.define [name, m]

  let diff s = s.diff

  let open_path ~param ~loc state path =
    merge state (T.open_diverge param.T.policy loc path)

  let from_env ?(diff=Summary.empty) env =
    { initial = env; diff; current= Env.extend env diff }

  let rec_approximate state l =
    List.fold_left (fun state {M2l.expr=_; name} ->
        match name with
        | None -> state
        | Some ename ->
          merge state (bind name @@ P.of_module ename @@ Module.mockup ename)
      ) state l

  let rec_patch y diff = Y.merge diff y

  let peek x = x
  let resolve param state
      ({edge; loc; level; path; within; _ }: path_in_context) =
    let edge =
      if not param.T.epsilon_dependencies then Some Deps.Edge.Normal
      else edge in
    let find = match within with
      | None -> Env.find
      | Some within -> Env.find_within within
    in
    match find ?edge loc level path state.current with
    | exception Not_found -> Error ()
    | x ->
      List.iter (fault param) x.msgs;
      debug "@[<hv>State: %a@ @[<hv 2>Resolving@ %a@ to@ %a@]@]@."
        Env.pp state.current Paths.S.pp path T.pp_answer x.main;
      Ok x


end

module type state = sig
  type state
  type env
  val resolve :
  Transforms.param -> state -> path_in_context -> (query, unit) result
  val merge : state -> state_diff -> state
  val bind_arg : state -> module_like Module.Arg.t -> state
  val is_alias : Transforms.param -> state -> Paths.Simple.t -> bool
  val restart : state -> state_diff -> state
  val bind_alias : state -> Name.t option -> Paths.Simple.t -> state_diff
  val diff : state -> state_diff
  val open_path :
    param:Transforms.param -> loc:Fault.loc -> state -> path -> state
  val from_env: ?diff:state_diff -> env -> state
  val rec_approximate: state -> _ M2l.bind list -> state

  val rec_patch: Summary.t -> state_diff -> state_diff

  (** to be deleted ?*)
  val peek: state_diff -> Summary.t
end
