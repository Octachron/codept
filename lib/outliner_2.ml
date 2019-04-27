open M2l
open M2ldiff

let (<++>) = (<+>)

type param = {
  policy: Fault.Policy.t;
  epsilon_dependencies: bool;
  transparent_extension_nodes: bool;
  transparent_aliases: bool
}

module M=Module
module Y = Summary
module F = Standard_faults
module P = M.Partial
module Env = Envt.Core

(* Remove deleted modules with `with A.B.C.D := …` *)
let rec remove_path_from path = function
  | M.Blank -> M.Blank
  | Divergence d ->
    Divergence { d with
                 before = remove_path_from path d.before;
                 after = remove_path_from_sig path d.after
               }
  | Exact defs -> Exact (remove_path_from_sig path defs)
and remove_path_from_sig path defs = match path with
  | [] -> defs
  | [a] -> { defs with modules = Name.Map.remove a defs.modules }
  | a :: rest ->
    let update: M.t -> _ = function
      | M.Alias _ | Namespace _ as x -> x
      | M m -> M.M { m with signature = remove_path_from rest m.signature }
    in
    { defs with modules= Name.Map.update a update defs.modules }

let with_deletions dels d =
  Paths.S.Set.fold remove_path_from dels d


let filename loc = fst loc


type 'a query_result =
  { main:'a; deps: Deps.t; msgs: Fault.t list }

type answer = Outliner.answer =
  | M of Module.m
  | Namespace of M.namespace_content


type level = Module.level = Module | Module_type
module S = M.Sig

let open_diverge_module policy loc x = let open P in
  match x.origin, x.result with
  | _, Blank | Phantom _, _ ->
    let kind =
      match x.origin with
      | First_class ->
        Fault.raise policy F.opened_first_class (loc,x.name);
        Module.Divergence.First_class_module
      | Unit _ -> Module.Divergence.External
      | Phantom (_,d) -> d.origin
      | Submodule | Arg | Namespace -> Module.Divergence.External in
    let point =
      { Module.Divergence.root = x.name; origin=kind; loc } in
    Y.View.see @@ S.merge
      (Divergence
         { before = S.empty; point; after = Module.Def.empty}
      )
      x.result
  | _, Divergence _ | _, Exact _ -> Y.View.see x.result

let open_diverge pol loc = function
  | M x -> open_diverge_module pol loc (P.of_module x)
  | Namespace {modules;_} -> (* FIXME: type error *)
    Y.View.see @@ M.Exact { M.Def.empty with modules }

let open_ pol loc x = open_diverge_module pol loc x


let pure r = {r; state = Y.empty }

class outliner param =
  let fault x = Fault.handle param.policy x in
  let raisef f t = fault (Fault.emit f t) in
  let of_partial loc p =
    match Y.of_partial p with
    | Error def -> raisef F.structure_expected (loc,p); def
    | Ok def -> def in
  let gen_include loc x =
    raisef F.included_first_class loc;
    () <++> of_partial loc x in
  let bind_summary name expr =
    let m = M.M (P.to_module ~origin:Submodule name expr) in
    Y.define [m] in
  let bind name expr = () <++> bind_summary name expr in
  let drop_arg loc (p:Module.Partial.t) = match p.args with
    | _ :: args -> { p with args }
    | [] ->
      if Module.Partial.is_exact p then
        (* we guessed the arg wrong *)
        raisef F.applied_structure (loc,p);
      p in
  object(self)
    inherit[
      <
      module_expr: P.t;
      module_type: P.t;
      path: answer;
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
    | {Outliner.msgs; main; deps=_} ->
      List.iter fault msgs;
      Ok main

  method annot _packed _access _values = ()

  method abstract = P.empty
  method apply loc f _x = drop_arg loc f
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
    | M x -> P.of_module x
    | Namespace _ -> assert false
  method alias = function
    | M x -> P.of_module x
    | Namespace n -> P.pseudo_module n

  method mt_with () dels mt =
    let result = with_deletions dels mt.P.result in
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
     Env.extend state (open_diverge param.policy loc path)

  method state_merge state state_diff = Env.extend state state_diff

  method access () = ()
  method access_add _ _ _ () = ()
  method access_init = ()

  method packed_init = ()
  method add_packed _ _ () = ()

  method path_expr_arg _ _ () = ()
  method path_expr x () = x
  method path_expr_arg_init = ()

  method sig_include loc e = gen_include loc e
  method mt_sig m = P.no_arg m


  method open_init = ()
  method open_add _ () = ()
  method mt_fun arg f = match arg with
    | None -> { f with P.args = None :: f.args }
    | Some {signature; name } ->
      { f with P.args = Some(P.to_arg name signature) :: f.args }


  method bind  name me = bind name me
  method bind_rec l =
    () <++>
    List.fold_left
      (fun y {name;expr} -> Y.merge y (bind_summary name expr)) Y.empty l

  method bind_rec_add name expr l =
    {name;expr} :: l <++> bind_summary name expr
  method bind_rec_init = []
  method bind_sig name m = bind name m
  method expr_ext _name _ = () <++> Y.empty
  method expr_include ~loc m = gen_include loc m
  method expr_open ~loc m = () <++> open_ param.policy loc m
  method ext_module _ = ()
  method ext_val _  = ()

  method mt_of x = x
  method state_bind_arg st {name;signature} =
    let m = M.M (P.to_module ~origin:Arg name signature) in
    Env.extend st (Y.define [m])

end

