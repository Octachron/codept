open M2l
open Mresult

module Y = Summary
module Def = Summary.Def
module P = Module.Partial

module M = Module
module Arg = M.Arg
module S = Module.Sig
module Faults = Standard_faults

type 'a query_result = { main:'a; msgs: (Fault.loc -> unit ) Fault.t list }

module type envt = sig
  type t
  val is_exterior: Paths.Simple.t -> t -> bool
  val find: ?edge:Deps.Edge.t -> Module.level -> Paths.Simple.t -> t ->
    Module.m query_result
  val (>>) : t -> Y.t -> t
  val resolve_alias: Paths.Simple.t -> t -> Name.t option
  val add_unit: t -> Module.t -> t
end

module type with_deps = sig
  type t
  val deps: t -> Deps.t
  val reset_deps: t -> unit
end

module type envt_with_deps = sig
  type t
  include envt with type t := t
  include with_deps with type t := t
end

module type s = sig
  type envt
  val m2l : Paths.P.t -> envt -> M2l.t -> (envt * Module.Sig.t, M2l.t) result
end

module type param = sig
  val policy: Fault.Policy.t
  val epsilon_dependencies: bool
  val transparent_extension_nodes: bool
  val transparent_aliases: bool
end

module Make(Envt:envt)(Param:param) = struct


  include Param
  let fault x = Fault.handle policy x

  let some x = Some x

  let find ?edge loc level path env =
    let edge =
      if not epsilon_dependencies then Some Deps.Edge.Normal
      else edge in
    let {main; msgs} = Envt.find ?edge level path env in
    List.iter (fun msg -> fault msg loc) msgs;
    main

  let drop_arg loc (p:Module.Partial.t) =  match  p.args with
      | _ :: args -> { p with args }
      | [] ->
        if Module.Partial.is_exact p then
          begin
            Fault.(handle policy Faults.applied_structure) loc p;
            p
            end
        else
          p (* we guessed the arg wrong *)


   let filename loc = fst loc

  let of_partial loc p =
    match Y.of_partial p with
    | Error def -> fault Faults.structure_expected loc p;
      def
    | Ok def -> def

  type level = Module.level = Module | Module_type
  let minor (path,_) module_expr str state m =
    let value l v = match str state v with
      | Ok _ -> l
      | Error h -> h :: l in
    let access n (loc,edge) m = match find ~edge (path,loc) Module [n] state with
      | _ -> m
      | exception Not_found -> Name.Map.add n (loc,edge) m in
    let packed l (p: _ Loc.ext) = match module_expr (path,p.loc) state p.data with
      | Ok _ -> l
      | Error h -> (Loc.create p.loc h) :: l in
    let values = List.fold_left value [] m.values in
    let access = Name.Map.fold access m.access Annot.Access.empty in
    let packed = List.fold_left packed [] m.packed in
    (* to do opaque *)
    if access = Annot.Access.empty && values = [] && packed = [] then
      Ok None
    else
      Error { access; values; packed }


  let mt_ident loc level state id =
    begin match find loc level id state with
    | x -> Ok(P.of_module x)
    | exception Not_found -> Error id
  end

  let epath loc state path =
    let paths = Paths.Expr.multiples path in
    let l = match paths with
      | a :: q ->
        (mt_ident loc Module_type state a) ::  List.map (mt_ident loc Module state) q
      | [] -> []
    in
    match l with
    | Ok x :: q when
        List.for_all (function Ok _ -> true | Error _ -> false) q ->
      Ok x
    | _ -> Error path

  let open_diverge loc x = let open Module in
    match x.origin, x.signature with
    | _, Blank | Phantom _, _ ->
      let kind =
        match x.origin with
        | First_class -> (fault Faults.opened_first_class loc x.name;
                          Divergence.First_class_module)
        | Unit _ -> Divergence.External
        | Phantom (_,d) -> d.origin
        | Submodule | Arg -> Divergence.External  (*FIXME?*) in
      let point = { Divergence.root = x.name; origin=kind; loc } in
      { Summary.visible = S.merge
            (Divergence
               { before = S.empty; point; after = Module.Def.empty}
            )
            x.signature;
        defined = Module.Sig.empty
      }
    | _, Divergence _ | _, Exact _ -> Y.sg_see x.signature


  let open_ loc state path =
    match find loc Module path state with
    | x ->
      Ok(open_diverge loc x)
    | exception Not_found -> Error (Open path)

  let gen_include loc unbox box i = match unbox i with
    | Error h -> Error (box h)
    | Ok fdefs ->
      if P.( fdefs.result = Blank (* ? *) && fdefs.origin = First_class ) then
        fault Standard_faults.included_first_class loc;
      Ok (Some (of_partial loc fdefs))

  let include_ loc state module_expr =
    gen_include loc (module_expr loc state) (fun i -> Include i)
  let sig_include loc state module_type = gen_include loc
      (module_type loc state) (fun i -> SigInclude i)

  let bind state module_expr {name;expr} =
    match module_expr state expr with
    | Error h -> Error ( Bind {name; expr = h} )
    | Ok d ->
      let m = M.M( P.to_module ~origin:Submodule name d ) in
      Ok (Some(Def.md m))

  let bind state module_expr (b: M2l.module_expr bind) =
    if not transparent_aliases then
      bind state module_expr b
    else
      match b.expr with
      | (Ident p:M2l.module_expr)
      | Constraint(Abstract, Alias p) when Envt.is_exterior p state ->
        let m = Module.Alias { name = b.name; path = p; phantom = None } in
        Ok ( Some (Def.md m) )
      | _ -> bind state module_expr b

  let bind_sig state module_type {name;expr} =
    match module_type state expr with
    | Error h -> Error ( Bind_sig {name; expr = h} )
    | Ok d ->
      let m = P.to_module ~origin:Submodule name d in
      Ok (Some(Def.sg (M.M m)))


  let bind_rec state module_expr module_type bs =
    let pair name expr = {name;expr} in
    let triple name me mt = name, me, mt in
    (* first we try to compute the signature of each argument using
       approximative signature *)
    let mockup ({name;_}:_ M2l.bind) = M.md @@ M.mockup name in
    let add_mockup defs arg = Envt.(>>) defs @@ Def.md @@ mockup arg in
    let state' = List.fold_left add_mockup state bs in
    let mapper {name;expr} =
      match expr with
      | Constraint(me,mt) ->
        mt |> module_type state'
        |> fmap (triple name me) (triple name me)
      | _ -> assert false (* recursive module are always constrained *)
    in
    let mts = List.map mapper bs in
    let undone (name, me, defs) = name, me, (Resolved defs:module_type) in
    let recombine (name,me,mt) = {name;expr = Constraint(me,mt) } in
    match all_done undone mts with
    | Error bs -> Error (Bind_rec (List.map recombine bs))
    | Ok defs ->
      (* if we did obtain the argument signature, we go on
         and try to resolve the whole recursive binding *)
      let add_arg defs (name, _me, arg) = Envt.(>>) defs @@ Def.md @@
        M.M (P.to_module ~origin:Arg name arg) in
      let state' = List.fold_left add_arg state defs in
      let mapper {name;expr} =
        expr |> module_expr state' |> fmap (pair name) (pair name) in
      let bs = List.map mapper bs in
      let undone { name; expr } = { name; expr = (Resolved expr:module_expr) } in
      match all_done undone bs with
      | Error bs -> Error (Bind_rec bs)
      | Ok defs ->
        let defs =
          List.fold_left
            ( fun defs {name;expr} ->
                Y.bind (M.M (P.to_module ~origin:Submodule name expr)) defs )
            Y.empty defs in
        Ok ( Some defs )

  let drop_state = function
    | Ok(_state,x) -> Ok x
    | Error _ as h -> h

   let functor_expr module_type (body_type,fn,demote) state args arg body =
    let ex_arg =
      match arg with
      | None -> Ok None
      | Some arg ->
        match module_type state arg.Arg.signature with
        | Error h -> Error (Some {Arg.name = arg.name; signature = h })
        | Ok d   -> Ok (Some(P.to_arg arg.name d)) in
    match ex_arg with
    | Error me -> Error (fn @@ List.fold_left demote {arg=me;body} args )
    | Ok arg ->
      let sg = Option.( arg >>| (fun m -> Def.md (M.M m) ) >< Y.empty ) in
      let state =  Envt.( state >> sg ) in
      match body_type state body with
      | Ok p  -> Ok { p with P.args = arg :: p.P.args }
      | Error me ->
        let arg = Option.(
            arg >>| fun arg ->
            { Arg.name = arg.name;
              signature:module_type= Resolved (P.of_module arg) }
          ) in
        Error (fn @@ List.fold_left demote {arg;body=me} args)


  let rec module_expr loc state (me:module_expr) = match me with
    | Abstract -> Ok P.empty
    | Unpacked -> Ok P.{ empty with result = Blank; origin = First_class }
    | Val m -> begin
        match minor loc module_expr (m2l @@ filename loc) state m with
        | Ok _ -> Ok { P.empty with result = Blank; origin = First_class }
        | Error h -> Error (Val h)
      end  (* todo : check warning *)
    | Ident i ->
      begin match find loc Module i state with
        | x -> Ok (P.of_module x)
        | exception Not_found -> Error (Ident i: module_expr)
      end
    | Apply {f;x} ->
      begin match module_expr loc state f, module_expr loc state x with
        | Ok f, Ok _ -> Ok (drop_arg loc f)
        | Error f, Error x -> Error (Apply {f;x} )
        | Error f, Ok d -> Error (Apply {f; x = Resolved d})
        | Ok f, Error x -> Error (Apply {f = Resolved f ;x} )
      end
    | Fun {arg;body} ->
      functor_expr (module_type loc) (module_expr loc, Build.fn, Build.demote_str)
        state [] arg body
    | Str [] -> Ok P.empty
    | Str[{data=Defs d;_ }] -> Ok (P.no_arg d.defined)
    | Resolved d -> Ok d
    | Str str -> Mresult.fmap (fun s -> Str s) P.no_arg @@
      drop_state @@ m2l (filename loc) state str
    | Constraint(me,mt) ->
      constraint_ loc state me mt
    | Open_me {opens=[]; resolved; expr } ->
      let state = Envt.( state >> resolved ) in
      module_expr loc state expr
    | Open_me {opens=a :: q ; resolved; expr } as me ->
      begin match find loc Module a state with
        | exception Not_found -> Error me
        | x ->
          let seen = open_diverge loc x in
          let resolved = Def.( resolved +| seen ) in
          module_expr loc state @@ Open_me {opens = q; resolved; expr }
      end
    | Extension_node n ->
      begin match extension loc state n with
        | Ok () -> Ok P.empty
        | Error h -> Error (Extension_node h)
      end

  and constraint_ loc state me mt =
    match module_expr loc state me, module_type loc state mt with
    | Ok _, (Ok _ as r) -> r
    | Ok me, Error mt ->
      Error (Constraint(Resolved me, mt) )
    | Error me, Ok mt ->
      Error (Constraint(me, Resolved mt) )
    | Error me, Error mt -> Error ( Constraint(me,mt) )

  and module_type loc state = function
    | Sig [] -> Ok P.empty
    | Sig [{data=Defs d;_}] -> Ok (P.no_arg d.defined)
    | Sig s -> Mresult.fmap (fun s -> Sig s) P.no_arg @@
      drop_state @@ signature (filename loc) state s
    | Resolved d -> Ok d
    | Ident id ->
      begin match epath loc state id with
        | Ok x -> Ok x
        | Error p -> Error (Ident p)
      end
    | Alias i ->
      begin match find loc Module i state with
        | x -> Ok (P.of_module x)
        | exception Not_found -> Error (Alias i)
      end
    | With w ->
      begin
        match module_type loc state w.body with
        | Error mt -> Error ( With { w with body = mt } )
        | Ok d ->
          let remove_from d = Name.Set.fold Name.Map.remove w.deletions d in
          let rec remove = function
            | M.Blank -> M.Blank
            | Exact d -> M.Exact { d with modules = remove_from d.modules }
            | Divergence p ->
              Divergence {
                p with before = remove p.before;
                       after = { p.after with modules = remove_from p.after.modules }
              } in
          Ok { d with result = remove d.result }
      end
    | Fun {arg;body} ->
      functor_expr (module_type loc)
        (module_type loc, Build.fn_sig, Build.demote_sig)
        state [] arg body
    | Of me -> of_ (module_expr loc state me)
    | Abstract -> Ok (P.empty)
    | Extension_node n ->
      begin match extension loc state n with
      | Ok () -> Ok (P.empty)
      | Error n -> Error (Extension_node n)
      end
  and of_ = function
    | Error me -> Error (Of me)
    | Ok d -> Ok d


  and m2l filename state = function
    | [] -> Ok (state, S.empty)
    | a :: q ->
      match expr filename state a with
      | Ok (Some defs)  ->
        begin match m2l filename Envt.( state >> defs ) q with
          | Ok (state,sg) ->  Ok (state, S.merge defs.defined sg)
          | Error q' -> Error ( snd @@ Normalize.all @@
                                Loc.nowhere (Defs defs) :: q')
        end
      | Ok None -> m2l filename state q
      | Error h -> Error ( snd @@ Normalize.all @@ h :: q)

  and signature filename state  = m2l filename state

  and expr file state e =
    let loc = file, e.loc in
    let reloc = Mresult.Error.fmap @@ Loc.create e.loc in
    match e.data with
    | Defs d -> Ok (Some d)
    | Open p -> reloc @@ Mresult.Ok.fmap some @@ open_ loc state p
    | Include i -> reloc @@ include_ loc state module_expr i
    | SigInclude i -> reloc @@ sig_include loc state module_type i
    | Bind b -> reloc @@ bind state (module_expr loc) b
    | Bind_sig b -> reloc @@ bind_sig state (module_type loc) b
    | Bind_rec bs -> reloc @@ bind_rec state (module_expr loc) (module_type loc) bs
    | Minor m -> reloc @@ Mresult.Error.fmap (fun m -> Minor m) @@
      minor loc module_expr (m2l file) state m
    | Extension_node n -> begin
        match extension loc state n with
        | Ok () -> Ok None
        | Error h -> Error (Loc.create e.loc @@ (Extension_node h: expression))
      end
  and extension loc state e =
    if not transparent_extension_nodes then
      ( fault Faults.extension_ignored loc e.name; Ok () )
    else
      begin
        let filename = fst loc in
        fault Faults.extension_traversed loc e.name;
        begin let open M2l in
          match e.extension with
          | Module m -> fmap (fun x -> { e with extension= Module x} ) ignore @@
            m2l filename state m
          | Val x -> fmap (fun x -> { e with extension=Val x}) ignore @@
            minor loc module_expr (m2l filename) state x
        end
      end

end
