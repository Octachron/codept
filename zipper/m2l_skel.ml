module M=Module
module Y = Summary
module F = Standard_faults
module P = M.Partial
module Env = Envt.Core

module Arg = M.Arg

module S = M.Sig
module T = Transforms


type 'state path_in_context =
  { loc: Fault.loc;
    edge:Deps.Edge.t option;
    level: Module.level;
    ctx: 'state;
    path: Paths.S.t
  }

type ('a,'b) with_state = { r:'a; state:'b}



let fault param x = Fault.handle param.T.policy x
let raisef param f t = fault param (Fault.emit f t)

  type module_expr = P.t
  type module_type = P.t
  type path = T.answer
  type m2l = S.t
  type state_diff = Y.t
  type state = { initial:Envt.Core.t; diff:state_diff; current:Envt.Core.t }
  type bind_rec = module_expr M2l.bind list
  type path_expr = path

let resolve param state ({edge; loc; level; path; _ }: _ path_in_context) =
  let edge =
    if not param.T.epsilon_dependencies then Some Deps.Edge.Normal
    else edge in
  match Env.find ?edge loc level path state.current with
  | exception Not_found -> Error ()
  | {T.msgs; main; deps=_} ->
    List.iter (fault param) msgs;
    Ok main


let abstract = P.empty
let apply param loc f _x = T.drop_arg param.T.policy loc f
let unpacked =
  P.{ empty with result = Blank; origin = First_class }

let me_val =
  { P.empty with result = Blank; origin = First_class }


let me_fun arg f =
  match arg with
  | None -> { f with P.args = None :: f.P.args }
  | Some {Arg.signature; name } ->
    { f with P.args = Some(P.to_arg name signature) :: f.args }

let me_ident = function
  | T.Namespace n -> P.pseudo_module n
  | T.M x -> P.of_module x

let str = P.no_arg

let ext_warn param (loc:Fault.loc) (name:string) =
  if not param.T.transparent_extension_nodes then
    raisef param F.extension_ignored (loc,name)
  else raisef param F.extension_traversed (loc,name)

let me_ext param ~loc name = ext_warn param loc name; P.empty

let sig_abstract = P.empty
let mt_abstract = P.empty

let mt_ident = function
  | T.M x -> P.of_module x
  | Namespace _ -> assert false
let alias = function
  | T.M x -> P.of_module x
  | T.Namespace n -> P.pseudo_module n

let m2l_add _loc expr defs = S.merge defs (Y.defined expr.state)
let m2l_init = S.empty


let sig_include param loc e = T.gen_include param.T.policy loc e
let mt_sig m = P.no_arg m
let mt_fun arg f = match arg with
  | None -> { f with P.args = None :: f.P.args }
  | Some {Arg.signature; name } ->
    { f with P.args = Some(P.to_arg name signature) :: f.args }
let mt_with () dels mt =
  let result = T.with_deletions dels mt.P.result in
  { mt with result }
let mt_ext param ~loc name _ = ext_warn param loc name; P.empty
let mt_of x = x

let bind name me = T.bind_summary Module name me

let bind_rec_add name expr y =
  Y.merge y (T.bind_summary Module name expr)
let bind_rec_init = Y.empty

let bind_sig name m = T.bind_summary Module_type name m
let expr_ext = Y.empty
let expr_include param ~loc m = T.gen_include param.T.policy loc m
let expr_open param ~loc m = T.open_ param.T.policy loc m

let state_merge state diff =
  { state with current = Env.extend state.current diff;
               diff = Y.merge state.diff diff }

let state_bind_arg st {Arg.name;signature} =
  let m = M.M (P.to_module ~origin:Arg name signature) in
  state_merge st (Y.define [m])

let state_is_alias param state s =
  param.T.transparent_aliases && Env.is_exterior s state.current

let state_restart state diff =
  { initial=state.initial;
    diff;
    current= Env.extend state.initial diff
  }

let bind_alias state name p =
  let path = Namespaced.of_path @@ Env.expand_path p state.current in
  let m = Module.Alias
      { name = name; path; weak=false; phantom = None } in
  Y.define [m]

let state_diff s = s.diff

let state_open_path ~param ~loc state path =
  state_merge state (T.open_diverge param.T.policy loc path)

(*
module R(P:Outliner.param) = Fold.Fold(Skel(P))
*)
