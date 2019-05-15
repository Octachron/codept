module M=Module
module Y = Summary
module F = Standard_faults
module P = M.Partial
module Env = Envt.Core

module Arg = M.Arg

module S = M.Sig
module T = Transforms

let fault param x = Fault.handle param.T.policy x
let raisef param f t = fault param (Fault.emit f t)

type module_like = P.t
type path = T.answer
type query = path T.query_result
type m2l = S.t
type state_diff = Y.t
type state = { initial:Envt.Core.t; diff:state_diff; current:Envt.Core.t }

type  path_in_context =
  { loc: Fault.loc;
    edge:Deps.Edge.t option;
    level: Module.level;
    ctx: state_diff;
    path: Paths.S.t
  }

let resolve param state ({edge; loc; level; path; _ }: path_in_context) =
  let edge =
    if not param.T.epsilon_dependencies then Some Deps.Edge.Normal
    else edge in
  match Env.find ?edge loc level path state.current with
  | exception Not_found -> Error ()
  | x ->
    List.iter (fault param) x.msgs;
    Ok x

let path x = x.T.main

let empty = P.empty

let abstract = empty
let apply param loc f = T.drop_arg param.T.policy loc f
let unpacked =
  { empty with result = Blank; origin = First_class }


let fn arg f =
  match arg with
  | None -> { f with P.args = None :: f.P.args }
  | Some {Arg.signature; name } ->
    { f with P.args = Some(P.to_arg name signature) :: f.args }


let str = P.no_arg

let ext param (loc:Fault.loc) (name:string) =
  if not param.T.transparent_extension_nodes then
    raisef param F.extension_ignored (loc,name)
  else raisef param F.extension_traversed (loc,name)

let ident = function
  | T.M x -> P.of_module x
  | T.Namespace n -> P.pseudo_module n

let m2l_add expr defs = S.merge defs (Y.defined expr)
let m2l_init = S.empty


let included param loc e = T.gen_include param.T.policy loc e


let m_with dels mt =
  let result = T.with_deletions dels mt.P.result in
  { mt with result }

let bind name me = T.bind_summary Module name me
let bind_sig name m = T.bind_summary Module_type name m


let bind_rec_add name expr y =
  Y.merge y (T.bind_summary Module name expr)
let bind_rec_init = Y.empty
let opened param ~loc m = T.open_ param.T.policy loc m
let empty_diff = Y.empty

module State = struct
  let merge state diff =
    { state with current = Env.extend state.current diff;
                 diff = Y.merge state.diff diff }

  let bind_arg st {Arg.name;signature} =
    let m = M.M (P.to_module ~origin:Arg name signature) in
    merge st (Y.define [m])

  let is_alias param state s =
    param.T.transparent_aliases && Env.is_exterior s state.current

  let restart state diff =
    { initial=state.initial;
      diff;
      current= Env.extend state.initial diff
    }

  let bind_alias state name p =
    let path = Namespaced.of_path @@ Env.expand_path p state.current in
    let m = Module.Alias
        { name = name; path; weak=false; phantom = None } in
    Y.define [m]

  let diff s = s.diff

  let open_path ~param ~loc state path =
    merge state (T.open_diverge param.T.policy loc path)
end

(*
module R(P:Outliner.param) = Fold.Fold(Skel(P))
*)
