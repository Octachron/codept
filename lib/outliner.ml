open M2l
open Mresult

module Y = Summary
module P = Module.Partial

module M = Module
module Arg = M.Arg
module S = Module.Sig
module Faults = Standard_faults

type 'a query_result =
  { main:'a; deps: Deps.t; msgs: (Fault.loc -> unit ) Fault.t list }

type answer =
  | M of Module.m
  | Namespace of M.namespace_content

module type envt = sig
  type t
  val eq: t -> t -> bool
  val find: ?edge:Deps.Edge.t -> Module.level -> Paths.Simple.t -> t ->
    answer query_result

  val (>>) : t -> Y.t -> t

  val is_exterior: Paths.Simple.t -> t -> bool
  val resolve_alias: Paths.Simple.t -> t -> Namespaced.t option
  val expand_path: Paths.Simple.t -> t -> Paths.Simple.t

  val add_unit: t -> ?namespace:Paths.Simple.t -> Module.t -> t
  val add_namespace: t -> Namespaced.t -> t

  val pp: Format.formatter -> t -> unit
end

module With_deps = struct
end

module type s = sig
  type envt
  val m2l : Paths.P.t -> envt -> M2l.t -> (envt * Deps.t * Module.Sig.t, M2l.t) result
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

  let with_deps d x = d, x
  let no_deps x = with_deps Deps.empty x
  let (<+>) (d, x) (d', y) = Deps.merge d d', x , y
  let more_deps deps (x:Y.t) = Some { x with deps = Deps.( deps + x.deps) }

  let _m2l_deps = function
    | Ok(_,d,_) -> d
    | Error ( Defs d :: _ ) -> d.deps
    | Error _ -> Deps.empty

  let ok x = no_deps (Ok x)
  let err x = no_deps (Error x)

  let find ?edge loc level path env =
    let edge =
      if not epsilon_dependencies then Some Deps.Edge.Normal
      else edge in
    let {main; deps; msgs} = Envt.find ?edge level path env in
    List.iter (fun msg -> fault msg loc) msgs;
    deps, main

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
    | a :: rest -> let open Option in
      let mods = defs.modules in
      Name.Map.find_opt a mods
      >>| ( function
          | Namespace _ | Alias _ -> defs
          | M m ->
            let m =
              M.M{ m with signature = remove_path_from rest m.signature }
            in
            { defs with modules = Name.Map.add a m mods }
        ) >< defs

  let with_deletions dels d =
    Paths.S.Set.fold remove_path_from dels d


  let filename loc = fst loc

  let of_partial loc p =
    match Y.of_partial p with
    | Error def -> fault Faults.structure_expected loc p;
      def
    | Ok def -> def

  type level = Module.level = Module | Module_type

  let access (path,_) state x =
    let access n (loc,edge) (d,m) = match find ~edge (path,loc) Module n state with
      | d', _ -> Deps.(d + d'), m
      | exception Not_found ->  d, Paths.S.Map.add n (loc,edge) m in
    let d, access = Paths.S.Map.fold access x (no_deps Annot.Access.empty) in
    if access = Annot.Access.empty then
      with_deps d (Ok ())
    else
      with_deps d (Error access)

  let minor (path,_ as loc) module_expr str state m =
    let value (d,l) v = match str state v with
      | Ok (_,deps,_) -> Deps.(d+deps), l
      | Error ({Loc.data=Defs defs; _} :: _ as h) ->
        Deps.( defs.deps + d), h :: l
      | Error h -> d, h :: l in
    let packed (d,l) (p: _ Loc.ext) =
      match module_expr (path,p.loc) state p.data with
      | d', Ok _ -> Deps. (d + d'), l
      | d', Error h -> Deps.( d + d'), (Loc.create p.loc h) :: l in
    let d1, values = List.fold_left value (Deps.empty, []) m.values in
    let d2, access = access loc state m.access in
    let d, packed = List.fold_left packed (Deps.(d1+d2),[]) m.packed in
    (* to do opaque *)
    with_deps d @@
    match access with
    | Ok () when values = [] && packed = [] -> Ok None
    | Ok () -> Error { access = Annot.Access.empty; values; packed }
    | Error access -> Error { access; values; packed }


  let mt_ident loc level state id =
    begin match find loc level id state with
      | d, M x -> with_deps d @@ Ok (P.of_module x)
      | d, Namespace _ -> with_deps d @@ (* FIXME? *) Error id
      | exception Not_found -> err id
  end

  let epath loc state path =
    let paths = Paths.Expr.multiples path in
    let d, mt, rest = match paths with
      | a :: q ->
        let d, mt = mt_ident loc Module_type state a in
        let resolve (d,l) x =
          let d', x = mt_ident loc Module state x in
          Deps.(d+d'), x::l in
        let d, rest = List.fold_left resolve (with_deps d []) q in
        d, Some mt, rest
      | [] -> Deps.empty, None, []
    in
    with_deps d @@
    match mt with
    | Some (Ok x) when List.for_all is_ok rest ->
      Ok x
    | _ -> Error path

  let open_diverge_module loc x = let open P in
    match x.origin, x.result with
    | _, Blank | Phantom _, _ ->
      let kind =
        match x.origin with
        | First_class -> (fault Faults.opened_first_class loc x.name;
                          Module.Divergence.First_class_module)
        | Unit _ -> Module.Divergence.External
        | Phantom (_,d) -> d.origin
        | Submodule | Arg | Namespace -> Module.Divergence.External in
      let point =
        { Module.Divergence.root = x.name; origin=kind; loc } in
      Y.View.see @@ Y.View.make @@ S.merge
            (Divergence
               { before = S.empty; point; after = Module.Def.empty}
            )
            x.result
    | _, Divergence _ | _, Exact _ ->
      Y.View.see @@ Y.View.make @@ x.result

  let open_diverge loc = function
    | M x -> open_diverge_module loc (P.of_module x)
    | Namespace {modules;_} -> (* FIXME: type error *)
      Y.View.(see @@ make @@ M.Exact { M.Def.empty with modules })

  let open_ loc x =
    Ok(open_diverge_module loc x)

  let gen_include loc unbox box i = match unbox i with
    | d, Error h -> with_deps d @@ Error (box h)
    | d, Ok fdefs -> with_deps d (
      if P.( fdefs.result = Blank (* ? *) && fdefs.origin = First_class ) then
        fault Standard_faults.included_first_class loc;
      Ok (Some (of_partial loc fdefs))
    )

  let include_ loc state module_expr =
    gen_include loc (module_expr loc state) (fun i -> Include i)
  let sig_include loc state module_type = gen_include loc
      (module_type loc state) (fun i -> SigInclude i)

  let bind state module_expr {name;expr} =
    match module_expr state expr with
    | deps, Error h -> deps, Error ( Bind {name; expr = h} )
    | deps, Ok d ->
      let m = M.M( P.to_module ~origin:Submodule name d ) in
      deps, Ok (Some(Y.define [m]))

  let bind state module_expr (b: M2l.module_expr bind) =
    match b.expr with
    | (Ident p:M2l.module_expr)
    | Constraint(Abstract, Alias p) when Envt.is_exterior p state ->
      begin
        let path = Namespaced.of_path @@ Envt.expand_path p state in
        let m = Module.Alias
            { name = b.name; path; weak=false; phantom = None } in
        let r = Ok ( Some (Y.define [m]) ) in
        if not transparent_aliases then
          (* trigger dependency tracking *)
          match bind state module_expr b with
          | deps, (Error _ as e) -> deps, e
          | deps, Ok _ -> deps, r
        else
          no_deps r
      end
    | _ -> bind state module_expr b


  let all_done_with_deps undone l =
    let deps, l = List.split l in
    let deps = List.fold_left Deps.merge Deps.empty deps in
    deps, all_done undone l

  let bind_sig state module_type {name;expr} =
    match module_type state expr with
    | deps, Error h -> deps, Error ( Bind_sig {name; expr = h} )
    | deps, Ok d ->
      let m = P.to_module ~origin:Submodule name d in
      deps, Ok (Some(Y.define ~level:M.Module_type [M.M m]))

  let bind_rec state module_expr module_type bs =
    let pair name expr = {name;expr} in
    let triple name me mt = name, me, mt in
    (* first we try to compute the signature of each argument using
       approximative signature *)
    let mockup ({name;_}:_ M2l.bind) = M.md @@ M.mockup name in
    let add_mockup defs arg =
      Envt.(>>) defs @@ Y.define [mockup arg] in
    let state' = List.fold_left add_mockup state bs in
    let mapper {name;expr} =
      match expr with
      | Constraint(me,mt) ->
        let d, mt =  module_type state' mt in
        d, fmap (triple name me) (triple name me) mt
      | _ -> assert false (* recursive module are always constrained *)
    in
    let mts = List.map mapper bs in
    let undone (name, me, defs) = name, me, (Resolved defs:module_type) in
    let recombine (name,me,mt) = {name;expr = Constraint(me,mt) } in
    match all_done_with_deps undone mts with
    | deps, Error bs -> deps, Error (Bind_rec (List.map recombine bs))
    | deps, Ok defs ->
      (* if we did obtain the argument signature, we go on
         and try to resolve the whole recursive binding *)
      let add_arg defs (name, _me, arg) =
        Envt.(>>) defs
        @@ Y.define [M.M (P.to_module ~origin:Arg name arg)] in
      let state' = List.fold_left add_arg state defs in
      let mapper {name;expr} =
        let d, me = module_expr state' expr in
        d, fmap (pair name) (pair name) me
      in
      let bs = List.map mapper bs in
      let undone { name; expr } = { name; expr = (Resolved expr:module_expr) } in
      match all_done_with_deps undone bs with
      | deps', Error bs -> Deps.( deps' + deps), Error (Bind_rec bs)
      | deps', Ok defs ->
        let defs =
          List.fold_left
            ( fun defs {name;expr} ->
                Y.bind (M.M (P.to_module ~origin:Submodule name expr)) defs )
            Y.empty defs in
        Deps.( deps + deps') , Ok ( Some defs )

  let drop_state = function
    | Ok(_state,d,x) -> d, Ok x
    | Error _ as h -> Deps.empty, h

   let functor_expr module_type (body_type,fn,demote) state args arg body =
    let deps, ex_arg =
      match arg with
      | None -> ok None
      | Some arg ->
        match module_type state arg.Arg.signature with
        | deps, Error h -> deps, Error (Some {Arg.name = arg.name; signature = h })
        | deps, Ok d   -> deps, Ok (Some(P.to_arg arg.name d)) in
    match ex_arg with
    | Error me -> deps, Error (fn @@ List.fold_left demote {arg=me;body} args )
    | Ok arg ->
      let sg =
        Option.( arg >>| (fun m -> Y.define [M.M m] ) >< Y.empty ) in
      let state =  Envt.( state >> sg ) in
      match body_type state body with
      | d, Ok p  -> Deps.( d + deps), Ok { p with P.args = arg :: p.P.args }
      | d, Error me ->
        let arg = Option.(
            arg >>| fun arg ->
            { Arg.name = arg.name;
              signature:module_type= Resolved (P.of_module arg) }
          ) in
        Deps.( d + deps), Error (fn @@ List.fold_left demote {arg;body=me} args)


  let rec module_expr loc state (me:module_expr) = match me with
    | Abstract -> ok P.empty
    | Unpacked -> ok P.{ empty with result = Blank; origin = First_class }
    | Val m -> begin
        match minor loc module_expr (m2l @@ filename loc) state m with
        | d, Ok _ -> with_deps d (Ok { P.empty with result = Blank; origin = First_class })
        | d, Error h -> with_deps d (Error (Val h: module_expr))
      end  (* todo : check warning *)
    | Ident i ->
      begin match find loc Module i state with
        | d, M x -> with_deps d (Ok(P.of_module x))
        | d, Namespace n -> with_deps d (Ok (P.pseudo_module n))
        | exception Not_found -> no_deps @@ Error (Ident i: module_expr)
      end
    | Apply {f;x} ->
      begin match module_expr loc state f <+> module_expr loc state x with
        | d, Ok f, Ok _ -> with_deps d (Ok (drop_arg loc f))
        | d, Error f, Error x -> with_deps d @@ Error (Apply {f;x} )
        | d, Error f, Ok x -> with_deps d @@ Error (Apply {f; x = Resolved x})
        | d, Ok f, Error x -> with_deps d @@ Error (Apply {f = Resolved f ;x} )
      end
    | Fun {arg;body} ->
      functor_expr (module_type loc) (module_expr loc, Build.fn, Build.demote_str)
        state [] arg body
    | Str [] -> ok P.empty
    | Str[{data=Defs d;_ }] -> ok (P.no_arg @@ Y.peek @@ Y.defined d)
    | Resolved d -> ok d
    | Str str ->
      let d , str = drop_state @@ m2l (filename loc) state str in
      d, Mresult.fmap (fun s -> Str s) P.no_arg str
    | Constraint(me,mt) ->
      constraint_ loc state me mt
    | Open_me {opens=[]; resolved; expr } ->
      let state = Envt.( state >> resolved ) in
      module_expr loc state expr
    | Open_me {opens=a :: q ; resolved; expr } as me ->
      begin match find loc Module a state with
        | exception Not_found -> err me
        | d, x ->
          let seen = open_diverge loc x in
          let resolved = Y.( resolved +| seen ) in
          let d', me =
            module_expr loc state @@ Open_me {opens = q; resolved; expr } in
          with_deps Deps.(d+d') me
      end
    | Extension_node n ->
      begin match extension loc state n with
        | d, Ok _ -> with_deps d (Ok P.empty)
        | d, Error h -> with_deps d (Error(Extension_node h: module_expr))
      end

  and constraint_ loc state me mt =
    match module_expr loc state me <+> module_type loc state mt with
    | d, Ok _, (Ok _ as r) -> d, r
    | d, Ok me, Error mt ->
      d, Error (Constraint(Resolved me, mt) )
    | d, Error me, Ok mt ->
      d, Error (Constraint(me, Resolved mt) )
    | d, Error me, Error mt -> d, Error ( Constraint(me,mt) )

  and module_type loc state = function
    | Sig [] -> ok P.empty
    | Sig [{data=Defs d;_}] -> ok (P.no_arg @@ Y.peek @@ Y.defined d)
    | Sig s ->
      let d, s = drop_state @@ signature (filename loc) state s in
      with_deps d @@ Mresult.fmap (fun s -> Sig s) P.no_arg s
    | Resolved d -> ok d
    | Ident id ->
      begin match epath loc state id with
        | d, Ok x -> with_deps d (Ok x)
        | d, Error p -> with_deps d (Error (Ident p))
      end
    | Alias i ->
      begin match find loc Module i state with
        | d, M x -> with_deps d (Ok (P.of_module x))
        | d, Namespace _ -> (* FIXME: type error *)
          with_deps d @@ Error(Alias i)
        | exception Not_found -> err (Alias i)
      end
    | With w ->
      begin match access loc state w.access with
        | d, Error access -> d, Error ( With { w with access } )
        | deps, Ok () ->
          begin
            match module_type loc state w.body with
            | d', Error mt ->
              with_deps Deps.( deps + d') @@
              Error ( With { w with body = mt } )
            | d', Ok d -> with_deps Deps.( deps + d') @@
              Ok { d with result = with_deletions w.deletions d.result }
          end
      end
        | Fun {arg;body} ->
      functor_expr (module_type loc)
        (module_type loc, Build.fn_sig, Build.demote_sig)
        state [] arg body
    | Of me -> of_ (module_expr loc state me)
    | Abstract -> ok (P.empty)
    | Extension_node n ->
      begin match extension loc state n with
      | deps, Ok _ -> deps, Ok (P.empty)
      | deps, Error n -> deps, Error (Extension_node n)
      end
  and of_ = function
    | deps, Error me -> deps, Error (Of me)
    | deps, Ok d -> deps, Ok d


  and m2l filename state = function
    | [] -> Ok (state, Deps.empty, S.empty)
    | a :: q ->
      match expr filename state a with
      | Ok (Some defs)  ->
        begin match m2l filename Envt.( state >> defs ) q with
          | Ok (state, d', sg) ->
            Ok (state, Deps.merge defs.deps d', S.merge (Y.peek @@ Y.defined defs) sg)
          | Error q' ->
            Error ( snd @@ Normalize.all @@
                    Loc.nowhere (Defs defs) :: q')
        end
      | Ok None -> m2l filename state q
      | Error (deps,h) ->
        let deps = Y.{ empty with deps } in
        Error ( snd @@ Normalize.all @@ Loc.nowhere (Defs deps) :: h :: q)

  and signature filename state  = m2l filename state

  and expr file state e =
    let loc = file, e.loc in
    let _reloc = Mresult.Error.fmap @@ Loc.create e.loc in
    let reloc deps = function
      | Error x -> Error(deps, Loc.create e.loc x)
      | Ok Some (d:Y.t) -> Ok (Some { d with deps = Deps.( deps + d.deps) })
      | Ok None -> Ok Y.(Some { empty with deps})
    in
    let reloc' (d,x) = reloc d x in
    match e.data with
    | Defs d -> Ok (Some d)
    | Open me ->
      let d, me = module_expr loc state me in
      reloc d
      @@ Mresult.fmap (fun x -> Open x) (more_deps d)
      @@ Mresult.Ok.bind (fun x -> open_ loc x)
      me
    | Include i -> reloc' @@ include_ loc state module_expr i
    | SigInclude i -> reloc' @@ sig_include loc state module_type i
    | Bind b -> reloc' @@ bind state (module_expr loc) b
    | Bind_sig b -> reloc' @@ bind_sig state (module_type loc) b
    | Bind_rec bs -> reloc' @@ bind_rec state (module_expr loc) (module_type loc) bs
    | Minor m ->
      let d, m = minor loc module_expr (m2l file) state m in
      reloc d @@ Mresult.Error.fmap (fun m -> Minor m) m
    | Extension_node n ->
      let d, ext = extension loc state n in
      reloc d @@ Mresult.Error.fmap (fun h -> (Extension_node h: expression)) ext
  and extension loc state e =
    if not transparent_extension_nodes then
      ( fault Faults.extension_ignored loc e.name; ok None )
    else
      begin
        let filename = fst loc in
        fault Faults.extension_traversed loc e.name;
        begin let open M2l in
          match e.extension with
          | Module m ->
            begin match m2l filename state m with
              | Ok(_,d,_) -> d, Ok None
              | Error x -> err { e with extension= Module x}
            end
          | Val x ->
            let deps, v =  minor loc module_expr (m2l filename) state x in
            with_deps deps
            @@ fmap (fun x -> { e with extension=Val x}) (fun _ -> None) v
        end
      end

end
