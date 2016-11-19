open M2l
open Result

module D = Definition
module Def = D.Def
module P = Partial_module

module M = Module
module S = Module.Sig

module type envt = sig
  type t
  val find: transparent:bool -> ?alias:bool -> M.level -> Npath.t -> t -> Module.t
  val (>>) : t -> M.signature -> t
  val add_module: t -> Module.t -> t
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
    let paths = Epath.multiples path in
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
        | Unit _ | Submodule | Arg | Rec -> ()
        | Alias _ -> ()
        | Extern -> () (* add a hook here? *)

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
      let defs = P.to_defs fdefs in
      Ok (Some defs)

  let include_ state module_expr =
    gen_include (module_expr state) (fun i -> Include i)
  let sig_include state module_type = gen_include
      (module_type state) (fun i -> SigInclude i)

  let aliased d = match d.P.origin with
    | Alias _ -> None
    | _ -> Some Module.Submodule

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


  let bind_rec state module_expr bs =
    let pair x y = x,y in
    let mockup ({name;_}:_ M2l.bind) =
      {M.origin = Rec; name; args = []; signature = S.empty } in
    let add_mockup defs arg = Envt.add_module defs @@ mockup arg in
    let state' = List.fold_left add_mockup state bs in
    let mapper {name;expr} =
      expr |> module_expr ?bind:(Some true) state'
      |> fmap (pair name) (pair name) in
    let bs = List.map mapper bs in
    let undone (name,defs) = name, (Resolved defs:module_expr) in
    let recombine (name,expr) = {name;expr} in
    match all_done undone bs with
    | Error bs -> Error (Bind_rec (List.map recombine bs))
    | Ok defs ->
      let defs =
        List.fold_left
          ( fun defs (name,d) ->
              D.bind (P.to_module ?origin:(aliased d) name d) defs )
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
              { p with origin = M.at_most p.origin @@ Alias (Npath.prefix i) } in
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
      functor_expr (module_expr ~bind:false,fn, demote_str) state [] arg body
    | Str [] -> Ok P.empty
    | Str[Defs d] -> Ok (P.no_arg d.defined)
    | Resolved d -> Ok d
    | Str str -> Result.fmap (fun s -> Str s) P.no_arg @@
      drop_state @@ m2l state str
    | Constraint(me,mt) ->
      constraint_ state me mt
    | Open_me {opens=[]; resolved; expr } ->
      let state = Envt.( state >> resolved.D.visible ) in
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
        | x -> Ok { (P.of_module x) with origin = Alias (Npath.prefix i) }
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
      functor_expr (module_type, fn_sig, demote_sig) state [] arg body
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
    Envt.t ->  D.t Arg.t option list -> arg
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
      let sg = Option.( arg >>| S.create >< S.empty ) in
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
        begin match m2l Envt.( state >> defs.D.visible ) q with
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
    | Bind_rec bs -> bind_rec state module_expr bs
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
