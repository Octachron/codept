open M2l
open Work

module D = Definition
module Def = D.Def
module P = Partial

module M = Module
module S = Module.Sig

module type envt = sig
  type t
  val find_partial: transparent:bool -> M.level -> Npath.t -> t -> t
  val find: transparent:bool -> ?alias:bool -> M.level -> Npath.t -> t -> Module.t
  val (>>) : t -> M.signature -> t
  val add_module: t -> Module.t -> t
end

module type param = sig
  val transparent_extension_nodes: bool
  val transparent_aliases: bool
end

module Envt = struct
  type t = M.signature

  let empty = S.empty

  let proj lvl env = match lvl with
    | M.Module -> env.M.modules
    | M.Module_type -> env.module_types

  let root_origin level path env =
    let a, lvl = match path with
      | []-> raise @@ Invalid_argument "Compute.Envt.root_origin: empty path"
      | [a] -> a, level
      | a :: _ -> a, M.Module in
    let m = Name.Map.find a @@ proj lvl env in
    m.origin

  let rec find ~transparent ?alias level path env =
    match path with
    | [] -> raise (Invalid_argument "Envt.find cannot find empty path")
    | [a] -> Name.Map.find a @@ proj level env
    | a :: q ->
      let m = Name.Map.find a env.modules in
      find ~transparent ?alias level q m.signature

  let find_partial ~transparent level path env =
    let m = find ~transparent ~alias:false level path env in
    m.signature

  let (>>) = Def.(+@)
  let add_module = S.add


end


module Tracing = struct

  type core = { env: Envt.t; protected: Name.set }
  type t = { env: Envt.t;
             protected: Name.set;
             deps: Name.set ref;
             cmi_deps: Name.set ref
           }

  let record n env = env.deps := Name.Set.add n !(env.deps)
  let record_cmi n env = env.cmi_deps := Name.Set.add n !(env.cmi_deps)


  let start protected = { protected; env = Envt.empty }

  let empty ()= {deps = ref Name.Set.empty; protected = Name.Set.empty;
               env = Envt.empty; cmi_deps = ref Name.Set.empty }

  let create ({ protected; env }:core) :t =
    { (empty ())  with env; protected }

  let deps env = env.deps

  let name path = List.hd @@ List.rev path

  let alias_chain start env a root = function
    | Module.Alias n when start-> Some n
    | Alias n -> Option.( root >>| fun r -> record_cmi r env; n )
    | Unit when start -> Some a
    | _ -> None

  let guard transparent f x = if transparent then f x else None

  let prefix level a env =
    match Name.Map.find a @@ Envt.proj level env.env with
    | exception Not_found -> a
    | { origin = Alias n; _ } -> n
    | _ -> a

  let rec find0 ~transparent start root level path env =
    match path with
    | [] -> raise (Invalid_argument "Envt.find cannot find empty path")
    | [a] ->
      let m = Name.Map.find a @@ Envt.proj level env.env in
      let root = guard transparent (alias_chain start env a root) m.origin in
      root, m
    | a :: q ->
      let m = Name.Map.find a env.env.modules in
      let root = guard transparent (alias_chain start env a root) m.origin in
      find0 ~transparent false root level q { env with env = m.signature }

  let check_alias name env =
    match (Name.Map.find name env.env.modules).origin with
    | Unit -> true
    | exception Not_found -> true
    | _ -> false

  let find ~transparent ?(alias=false) level path env =
    match find0 ~transparent true None level path env with
    | Some root, x when not transparent || not alias
      ->
      ( if check_alias root env then record root env; x)
    | Some _, x
    | None, x -> x
    | exception Not_found ->
      let root = prefix level (List.hd path) env in
      if Name.Set.mem root env.protected then
        raise Not_found
      else begin
        if not alias then record root env;
        Module.create ~origin:M.Extern (name path) S.empty
      end
  (* | Not_found -> raise Not_found *)

  let find_partial ~transparent level path core =
    let m = find ~transparent ~alias:false level path core in
    let env = m.signature in
    { (empty()) with env }

  let (>>) e1 sg = { e1 with env = Envt.( e1.env >> sg) }

  let add_module e m = { e with env = S.add e.env m }
  let add_core (c:core) m = { c with env = S.add c.env m }
end



let rec basic = function
  | [] -> []
  | a :: q -> match Reduce.expr a with
    | Done (Some d) -> Defs d :: basic q
    | Halted h -> h :: q
    | Done None -> basic q

module Make(Envt:envt)(Param:param) = struct

  include Param
  let find = Envt.find ~transparent:transparent_aliases
  let find_partial = Envt.find_partial ~transparent:transparent_aliases


  type level = Module.level = Module | Module_type
  let minor module_expr str state m =
    let value l v = match str state v with
      | Done _ -> l
      | Halted h -> h :: l in
    let access n m = match find Module [n] state with
      | _ -> m
      | exception Not_found -> Name.Set.add n m in
    let packed l p = match module_expr state p with
      | Done _ -> l
      | Halted h -> h :: l in
    let values = List.fold_left value [] m.values in
    let access = Name.Set.fold access m.access Name.Set.empty in
    let packed = List.fold_left packed [] m.packed in
    (* to do opaque *)
    if access = Name.Set.empty && values = [] && packed = [] then
      Done None
    else
      Halted { access; values; packed }


  let mt_ident level state id =  begin match find level id state with
    | x -> Done(P.of_module x)
    | exception Not_found -> Halted id
  end

  let epath state path =
    let paths = Epath.multiples path in
    let l = match paths with
    | a :: q ->
      (mt_ident Module_type state a) ::  List.map (mt_ident Module state) q
    | [] -> []
    in
    match l with
    | Done x :: q when
        List.for_all (function Done _ -> true | Halted _ -> false) q ->
      Done x
    | _ -> Halted path

  let open_ state path =
    match find Module path state with
    | x ->
      if x.signature = S.empty then
        begin match x.origin with
          | First_class -> Warning.opened_first_class x.name
          | Unit | Submodule | Arg | Rec -> ()
          | Alias _ -> ()
          | Extern -> () (* add a hook here? *)
        end;
      Done (Some( D.sg_see x.signature ))
    | exception Not_found -> Halted (Open path)

  let gen_include unbox box i = match unbox i with
    | Halted h -> Halted (box h)
    | Done fdefs ->
      if P.( fdefs.result = S.empty (* ? *) && fdefs.origin = First_class ) then
        Warning.included_first_class ();
      let defs = P.to_defs fdefs in
      Done (Some defs)

  let include_ state module_expr =
    gen_include (module_expr state) (fun i -> Include i)
  let sig_include state module_type = gen_include
      (module_type state) (fun i -> SigInclude i)

  let aliased d = match d.P.origin with
    | Alias _ -> None
    | _ -> Some Module.Submodule

  let bind state module_expr {name;expr} =
    match module_expr ?bind:(Some true) state expr with
    | Halted h -> Halted ( Bind {name; expr = h} )
    | Done d ->
      let m = P.to_module ?origin:(aliased d) name d in
      Done (Some(Def.md m))

  let bind_sig state module_type {name;expr} =
    match module_type state expr with
    | Halted h -> Halted ( Bind_sig {name; expr = h} )
    | Done d ->
      let m = P.to_module ?origin:(aliased d) name d in
      Done (Some(Def.sg m))


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
    | Halted bs -> Halted (Bind_rec (List.map recombine bs))
    | Done defs ->
      let defs =
        List.fold_left
          ( fun defs (name,d) ->
              D.bind (P.to_module ?origin:(aliased d) name d) defs )
          D.empty defs in
      Done ( Some defs )

  let drop_state = function
    | Done(_state,x) -> Done x
    | Halted _ as h -> h

  let rec module_expr ?(bind=false) state (me:module_expr) = match me with
    | Abstract -> Done P.empty
    | Unpacked -> Done P.{ empty with origin = First_class }
    | Val m -> begin
        match minor module_expr m2l state m with
        | Done _ -> Done { P.empty with origin = First_class }
        | Halted h -> Halted (Val h)
      end  (* todo : check warning *)
    | Ident i ->
      begin match find ~alias:bind Module i state with
        | x ->
          let p = P.of_module x in
          let p = if P.is_functor p || not bind then p
            else
              { p with origin = Alias (Npath.prefix i) } in
          Done p
        | exception Not_found -> Halted (Ident i: module_expr)
      end
    | Apply {f;x} ->
      begin match module_expr state f, module_expr state x with
        | Done f, Done _ -> Done (P.drop_arg f)
        | Halted f, Halted x -> Halted (Apply {f;x} )
        | Halted f, Done d -> Halted (Apply {f; x = Resolved d})
        | Done f, Halted x -> Halted (Apply {f = Resolved f ;x} )
      end
    | Fun {arg;body} ->
      functor_expr (module_expr ~bind:false,fn, demote_str) state [] arg body
    | Str [] -> Done P.empty
    | Str[Defs d] -> Done (P.no_arg d.defined)
    | Resolved d -> Done d
    | Str str -> Work.fmap (fun s -> Str s) P.no_arg @@
      drop_state @@ m2l state str
    | Constraint(me,mt) ->
      constraint_ state me mt
    | Extension_node n ->
      begin match extension state n with
        | Done () -> Done P.empty
        | Halted h -> Halted (Extension_node h)
      end

  and constraint_ state me mt =
    match module_expr state me, module_type state mt with
    | Done _, (Done _ as r) -> r
    | Done me, Halted mt ->
      Halted (Constraint(Resolved me, mt) )
    | Halted me, Done mt ->
      Halted (Constraint(me, Resolved mt) )
    | Halted me, Halted mt -> Halted ( Constraint(me,mt) )

  and module_type state = function
    | Sig [] -> Done P.empty
    | Sig [Defs d] -> Done (P.no_arg d.defined)
    | Sig s -> Work.fmap (fun s -> Sig s) P.no_arg @@
      drop_state @@ signature state s
    | Resolved d -> Done d
    | Ident id ->
      begin match epath state id with
        | Done x -> Done x
        | Halted p -> Halted (Ident p)
      end
    | Alias i ->
      begin match find Module i state with
        | x -> Done { (P.of_module x) with origin = Alias (Npath.prefix i) }
        | exception Not_found -> Halted (Alias i)
      end
    | With w ->
      begin
        match module_type state w.body with
        | Halted mt -> Halted ( With { w with body = mt } )
        | Done d ->
          let modules =
            Name.Set.fold Name.Map.remove w.deletions d.result.modules in
          let d = { d with result = { d.result with modules} } in
          Done d
      end
    | Fun {arg;body} ->
      functor_expr (module_type, fn_sig, demote_sig) state [] arg body
    | Of me -> of_ (module_expr state me)
    | Abstract -> Done (P.empty)
    | Extension_node n ->
      begin match extension state n with
      | Done () -> Done (P.empty)
      | Halted n -> Halted (Extension_node n)
      end
  and of_ = function
    | Halted me -> Halted (Of me)
    | Done d -> Done d

  and functor_expr: 'k. (Envt.t -> 'k -> ('k, P.t) M2l.Work.t)
                    * ('k M2l.fn -> 'k)
                    * ('k M2l.fn -> D.t Arg.t option -> 'k M2l.fn) ->
    Envt.t ->  D.t Arg.t option list -> arg
    -> 'k -> ('k, P.t) M2l.Work.t =
    fun (body_type,fn,demote) state args arg body ->
    let ex_arg =
      match arg with
      | None -> Done None
      | Some arg ->
        match module_type state arg.Arg.signature with
        | Halted h -> Halted (Some {Arg.name = arg.name; signature = h })
        | Done d   -> Done (Some(P.to_arg arg.name d)) in
    match ex_arg with
    | Halted me -> Halted (fn @@ List.fold_left demote {arg=me;body} args )
    | Done arg ->
      let sg = Option.( arg >>| S.create >< S.empty ) in
      let state =  Envt.( state >> sg ) in
      match body_type state body with
      | Done p  -> Done { p with args = arg :: p.args }
      | Halted me ->
        let arg = Option.(
            arg >>| fun arg ->
            { Arg.name = arg.name;
              signature:module_type= Resolved (P.of_module arg) }
          ) in
        Halted (fn @@ List.fold_left demote {arg;body=me} args)

  and m2l state = function
    | [] -> Done (state, S.empty)
    | a :: q ->
      match expr state a with
      | Done (Some defs)  ->
        begin match m2l Envt.( state >> defs.D.visible ) q with
          | Done (state,sg) ->  Done (state, S.merge defs.defined sg)
          | Halted q' -> Halted ( snd @@ Normalize.all @@ Defs defs :: q')
        end
      | Done None -> m2l state q
      | Halted h -> Halted ( snd @@ Normalize.all @@ h :: q)

  and signature state  = m2l state

  and expr state =
    function
    | Defs d -> Done (Some d)
    | Open p -> open_ state p
    | Include i -> include_ state module_expr i
    | SigInclude i -> sig_include state module_type i
    | Bind b -> bind state module_expr b
    | Bind_sig b -> bind_sig state module_type b
    | Bind_rec bs -> bind_rec state module_expr bs
    | Minor m -> Work.fmap_halted (fun m -> Minor m) @@
      minor module_expr m2l state m
    | Extension_node n -> begin
        match extension state n with
        | Done () -> Done None
        | Halted h -> Halted (Extension_node h)
      end
  and extension state e =
    if not transparent_extension_nodes then Done ()
    else
      begin let open M2l in
        match e.extension with
        | Module m -> fmap (fun x -> { e with extension= Module x} ) ignore @@
          m2l state m
        | Val x -> fmap (fun x -> { e with extension=Val x}) ignore @@
          minor module_expr m2l state x
      end

end

module Sg = Make(Envt)
module Tr = Make(Tracing)
