open M2l
open M2ldiff

let (<++>) = (<+>)


module M=Module
module Y = Summary
module F = Standard_faults
module P = M.Partial
module Env = Envt.Core


module S = M.Sig
module T = Transforms

let pure r = {r; state = Y.empty }
let ret x = () <++> x

class outliner param =
  let fault x = Fault.handle param.T.policy x in
  let raisef f t = fault (Fault.emit f t) in

  object(self)
    inherit[
      <
      module_expr: P.t;
      module_type: P.t;
      path: T.answer;
      annotation: unit;
      expr: unit;
      m2l: S.t;
      state: Envt.Core.t;
      state_diff: Y.t;
      opens:unit;
      ..
      >

           ] fold

  method resolve {edge; loc; level; path; state} =
    let edge =
      if not param.epsilon_dependencies then Some Deps.Edge.Normal
      else edge in
    match Envt.Core.find ?edge loc level path state with
    | exception Not_found -> Error ()
    | {T.msgs; main; deps=_} ->
      List.iter fault msgs;
      Ok main

  method annot _packed _access _values = ()

  method abstract = P.empty
  method apply loc f _x = T.drop_arg param.policy loc f
  method unpacked =
    P.{ empty with result = Blank; origin = First_class }
  method open_me _ x = x
  method minor _annot = pure ()
  method me_val () =
    { P.empty with result = Blank; origin = First_class }


  method me_fun arg f =
    match arg with
    | None -> { f with P.args = None :: f.args }
    | Some {signature; name } ->
      { f with P.args = Some(P.to_arg name signature) :: f.args }

  method me_ident = function
    | Namespace n -> P.pseudo_module n
    | M x -> P.of_module x

  method str = P.no_arg

  method ext_warn (loc:Fault.loc) (name:string) =
    if not param.transparent_extension_nodes then
      raisef F.extension_ignored (loc,name)
    else raisef F.extension_traversed (loc,name)

  method me_ext ~loc name () =
    self#ext_warn loc name;
    P.empty

  method me_constraint _ mt = mt
  method sig_abstract = P.empty
  method mt_abstract = P.empty

  method mt_ident = function
    | T.M x -> P.of_module x
    | Namespace _ -> assert false
  method alias = function
    | M x -> P.of_module x
    | Namespace n -> P.pseudo_module n

  method mt_with () dels mt =
    let result = T.with_deletions dels mt.P.result in
    { mt with result }
  method mt_ext ~loc name _ =
    self#ext_warn loc name;
    P.empty


  method m2l_add _loc expr defs = S.merge defs (Y.defined expr.state)
  method m2l_init = S.empty
  method m2l x = x

  method values _ = ()
  method value_init = ()
  method value_add _ () = ()

  method state_open_path ~loc state path =
     Env.extend state (T.open_diverge param.policy loc path)

  method state_merge state state_diff = Env.extend state state_diff

  method access () = ()
  method access_add _ _ _ () = ()
  method access_init = ()

  method packed_init = ()
  method add_packed _ _ () = ()

  method path_expr_arg _ _ () = ()
  method path_expr x () = x
  method path_expr_arg_init = ()

  method sig_include loc e = ret (T.gen_include param.policy loc e)
  method mt_sig m = P.no_arg m


  method open_init = ()
  method open_add _ () = ()
  method mt_fun arg f = match arg with
    | None -> { f with P.args = None :: f.args }
    | Some {signature; name } ->
      { f with P.args = Some(P.to_arg name signature) :: f.args }


  method bind  name me = ret (T.bind_summary Module name me)
  method bind_rec l =
    ret @@
    List.fold_left
      (fun y {name;expr} -> Y.merge y (T.bind_summary Module name expr))
      Y.empty l


  method bind_alias state name p =
    let path = Namespaced.of_path @@ Env.expand_path p state in
    let m = Module.Alias
        { name = name; path; weak=false; phantom = None } in
    ret (Y.define [m])

  method bind_rec_add name expr l =
    {name;expr} :: l <++> T.bind_summary Module name expr
  method bind_rec_init = []
  method bind_sig name m = ret (T.bind_summary Module_type name m)
  method expr_ext _name _ = ret Y.empty
  method expr_include ~loc m = ret (T.gen_include param.policy loc m)
  method expr_open ~loc m = ret (T.open_ param.policy loc m)
  method ext_module _ = ()
  method ext_val _  = ()

  method mt_of x = x
  method state_bind_arg st {name;signature} =
    let m = M.M (P.to_module ~origin:Arg name signature) in
    Env.extend st (Y.define [m])

  method state_is_alias state s =
    param.transparent_aliases && Env.is_exterior s state

end

