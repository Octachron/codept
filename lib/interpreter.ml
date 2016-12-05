open M2l
open Result

module D = Definition
module Def = D.Def
module P = Module.Partial

module M = Module
module Arg = M.Arg
module S = Module.Sig

module type envt = sig
  type t
  val find: transparent:bool -> ?alias:bool ->
    Module.level -> Paths.Simple.t -> t -> Module.t
  val (>>) : t -> D.t -> t
  val add_module: t -> Module.t -> t
end

module type with_deps = sig
  type t
  val deps: t -> Paths.Pkg.set
  val reset_deps: t -> unit
end

module type envt_with_deps = sig
  type t
  include envt with type t := t
  include with_deps with type t := t
end

module type s = sig
  type envt
  val m2l : envt -> M2l.t -> (envt * Module.Sig.t, M2l.t) result
end

module type param = sig
  val transparent_extension_nodes: bool
  val transparent_aliases: bool
end

module Make(Envt:envt)(Param:param) = struct

  include Param
  let find = Envt.find ~transparent:transparent_aliases


  type level = Module.level = Module | Module_type
  let minor module_expr str state m =
    let value l v = match str state v with
      | Ok _ -> l
      | Error h -> h :: l in
    let access n m = match find Module [n] state with
      | _ -> m
      | exception Not_found -> Name.Set.add n m in
    let packed l p = match module_expr state p with
      | Ok _ -> l
      | Error h -> h :: l in
    let values = List.fold_left value [] m.values in
    let access = Name.Set.fold access m.access Name.Set.empty in
    let packed = List.fold_left packed [] m.packed in
    (* to do opaque *)
    if access = Name.Set.empty && values = [] && packed = [] then
      Ok None
    else
      Error { access; values; packed }


  let mt_ident level state id =
    begin match find level id state with
    | x -> Ok(P.of_module x)
    | exception Not_found -> Error id
  end

  let epath state path =
    let paths = Paths.Expr.multiples path in
    let l = match paths with
      | a :: q ->
        (mt_ident Module_type state a) ::  List.map (mt_ident Module state) q
      | [] -> []
    in
    match l with
    | Ok x :: q when
        List.for_all (function Ok _ -> true | Error _ -> false) q ->
      Ok x
    | _ -> Error path

  let warn_open x = let open Module in
      if x.signature = S.empty then
        match x.origin with
        | First_class -> Warning.opened_first_class x.name
        | Unit _ | Submodule | Arg -> ()
        | Alias _ -> ()

  let open_ state path =
    match find Module path state with
    | x -> warn_open x;
      Ok (Some( D.sg_see x.signature ))
    | exception Not_found -> Error (Open path)

  let gen_include unbox box i = match unbox i with
    | Error h -> Error (box h)
    | Ok fdefs ->
      if P.( fdefs.result = S.empty (* ? *) && fdefs.origin = First_class ) then
        Warning.included_first_class ();
      let defs = D.of_partial fdefs in
      Ok (Some defs)

  let include_ state module_expr =
    gen_include (module_expr state) (fun i -> Include i)
  let sig_include state module_type = gen_include
      (module_type state) (fun i -> SigInclude i)

  let aliased d = match d.P.origin with
    | Alias _ -> None
    | _ -> Some Module.Origin.Submodule

  let bind state module_expr {name;expr} =
    match module_expr ?bind:(Some true) state expr with
    | Error h -> Error ( Bind {name; expr = h} )
    | Ok d ->
      let m = P.to_module ?origin:(aliased d) name d in
      Ok (Some(Def.md m))

  let bind_sig state module_type {name;expr} =
    match module_type state expr with
    | Error h -> Error ( Bind_sig {name; expr = h} )
    | Ok d ->
      let m = P.to_module ?origin:(aliased d) name d in
      Ok (Some(Def.sg m))


  let bind_rec state module_expr module_type bs =
    let pair name expr = {name;expr} in
    let triple name me mt = name, me, mt in
    (* first we try to compute the signature of each argument using
       approximative signature *)
    let mockup ({name;_}:_ M2l.bind) =
      {M.name;
       origin = Submodule;
       precision = Unknown;
       args = [];
       signature = S.empty } in
    let add_mockup defs arg = Envt.add_module defs @@ mockup arg in
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
      let add_arg defs (name, _me, arg) = Envt.add_module defs @@
        P.to_module ~origin:Arg name arg in
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
                D.bind (P.to_module ?origin:(aliased expr) name expr) defs )
            D.empty defs in
        Ok ( Some defs )

  let drop_state = function
    | Ok(_state,x) -> Ok x
    | Error _ as h -> h

  let rec module_expr ?(bind=false) state (me:module_expr) = match me with
    | Abstract -> Ok P.empty
    | Unpacked -> Ok P.{ empty with origin = First_class }
    | Val m -> begin
        match minor module_expr m2l state m with
        | Ok _ -> Ok { P.empty with origin = First_class }
        | Error h -> Error (Val h)
      end  (* todo : check warning *)
    | Ident i ->
      begin match find ~alias:bind Module i state with
        | x ->
          let p = P.of_module x in
          let p = if P.is_functor p || not bind then p
            else
              { p with origin = Alias p.origin } in
          Ok p
        | exception Not_found -> Error (Ident i: module_expr)
      end
    | Apply {f;x} ->
      begin match module_expr state f, module_expr state x with
        | Ok f, Ok _ -> Ok (P.drop_arg f)
        | Error f, Error x -> Error (Apply {f;x} )
        | Error f, Ok d -> Error (Apply {f; x = Resolved d})
        | Ok f, Error x -> Error (Apply {f = Resolved f ;x} )
      end
    | Fun {arg;body} ->
      functor_expr (module_expr ~bind:false, Build.fn, Build.demote_str)
        state [] arg body
    | Str [] -> Ok P.empty
    | Str[Defs d] -> Ok (P.no_arg d.defined)
    | Resolved d -> Ok d
    | Str str -> Result.fmap (fun s -> Str s) P.no_arg @@
      drop_state @@ m2l state str
    | Constraint(me,mt) ->
      constraint_ state me mt
    | Open_me {opens=[]; resolved; expr } ->
      let state = Envt.( state >> resolved ) in
      module_expr state expr
    | Open_me {opens=a :: q ; resolved; expr } as me ->
      begin match find Module a state with
        | exception Not_found -> Error me
        | x -> warn_open x;
          let resolved = Def.( resolved +| D.sg_see x.signature ) in
          module_expr state @@ Open_me {opens = q; resolved; expr }
      end
    | Extension_node n ->
      begin match extension state n with
        | Ok () -> Ok P.empty
        | Error h -> Error (Extension_node h)
      end

  and constraint_ state me mt =
    match module_expr state me, module_type state mt with
    | Ok _, (Ok _ as r) -> r
    | Ok me, Error mt ->
      Error (Constraint(Resolved me, mt) )
    | Error me, Ok mt ->
      Error (Constraint(me, Resolved mt) )
    | Error me, Error mt -> Error ( Constraint(me,mt) )

  and module_type state = function
    | Sig [] -> Ok P.empty
    | Sig [Defs d] -> Ok (P.no_arg d.defined)
    | Sig s -> Result.fmap (fun s -> Sig s) P.no_arg @@
      drop_state @@ signature state s
    | Resolved d -> Ok d
    | Ident id ->
      begin match epath state id with
        | Ok x -> Ok x
        | Error p -> Error (Ident p)
      end
    | Alias i ->
      begin match find Module i state with
        | x ->
          let m = P.of_module x in
          Ok { m with origin = Alias m.origin }
        | exception Not_found -> Error (Alias i)
      end
    | With w ->
      begin
        match module_type state w.body with
        | Error mt -> Error ( With { w with body = mt } )
        | Ok d ->
          let modules =
            Name.Set.fold Name.Map.remove w.deletions d.result.modules in
          let d = { d with result = { d.result with modules} } in
          Ok d
      end
    | Fun {arg;body} ->
      functor_expr (module_type, Build.fn_sig, Build.demote_sig) state [] arg body
    | Of me -> of_ (module_expr state me)
    | Abstract -> Ok (P.empty)
    | Extension_node n ->
      begin match extension state n with
      | Ok () -> Ok (P.empty)
      | Error n -> Error (Extension_node n)
      end
  and of_ = function
    | Error me -> Error (Of me)
    | Ok d -> Ok d

  and functor_expr: 'k. (Envt.t -> 'k -> (P.t, 'k) result)
                    * ('k M2l.fn -> 'k)
                    * ('k M2l.fn -> D.t Arg.t option -> 'k M2l.fn) ->
    Envt.t ->  D.t Arg.t option list -> module_type Arg.t option
    -> 'k -> (P.t, 'k) result =
    fun (body_type,fn,demote) state args arg body ->
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
      let sg = Option.( arg >>| Def.md >< D.empty ) in
      let state =  Envt.( state >> sg ) in
      match body_type state body with
      | Ok p  -> Ok { p with args = arg :: p.args }
      | Error me ->
        let arg = Option.(
            arg >>| fun arg ->
            { Arg.name = arg.name;
              signature:module_type= Resolved (P.of_module arg) }
          ) in
        Error (fn @@ List.fold_left demote {arg;body=me} args)

  and m2l state = function
    | [] -> Ok (state, S.empty)
    | a :: q ->
      match expr state a with
      | Ok (Some defs)  ->
        begin match m2l Envt.( state >> defs ) q with
          | Ok (state,sg) ->  Ok (state, S.merge defs.defined sg)
          | Error q' -> Error ( snd @@ Normalize.all @@ Defs defs :: q')
        end
      | Ok None -> m2l state q
      | Error h -> Error ( snd @@ Normalize.all @@ h :: q)

  and signature state  = m2l state

  and expr state =
    function
    | Defs d -> Ok (Some d)
    | Open p -> open_ state p
    | Include i -> include_ state module_expr i
    | SigInclude i -> sig_include state module_type i
    | Bind b -> bind state module_expr b
    | Bind_sig b -> bind_sig state module_type b
    | Bind_rec bs -> bind_rec state module_expr module_type bs
    | Minor m -> Result.fmap_error (fun m -> Minor m) @@
      minor module_expr m2l state m
    | Extension_node n -> begin
        match extension state n with
        | Ok () -> Ok None
        | Error h -> Error (Extension_node h)
      end
  and extension state e =
    if not transparent_extension_nodes then Ok ()
    else
      begin let open M2l in
        match e.extension with
        | Module m -> fmap (fun x -> { e with extension= Module x} ) ignore @@
          m2l state m
        | Val x -> fmap (fun x -> { e with extension=Val x}) ignore @@
          minor module_expr m2l state x
      end

end
