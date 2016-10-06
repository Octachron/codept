open M2l
open Work

module D = Definition
module Def = D.Def
module P = Partial

module M = Module
module S = Module.Sig

module type envt = sig
  type t
  val find: M.level -> Npath.t -> t -> Module.t
  val (>>) : t -> M.signature -> t
  val add_module: t -> Module.t -> t
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

  let rec find level path env =
    match path with
    | [] -> raise (Invalid_argument "Envt.find cannot find empty path")
    | [a] -> Name.Map.find a @@ proj level env
    | a :: q ->
      let m = Name.Map.find a env.modules in
      find level q m.signature

  let (>>) = Def.(+@)
  let add_module = S.add


end


module Tracing = struct

  type core = { env: Envt.t; protected: Name.set }
  type t = { env: Envt.t; protected: Name.set; deps: Name.set ref }

  let record n env = env.deps := Name.Set.add n !(env.deps)

  let start protected = { protected; env = Envt.empty }

  let create ({ protected; env }:core) :t =
    { env; protected; deps = ref Name.Set.empty }

  let deps env = env.deps

  let name path = List.hd @@ List.rev path

  let find level path env =
    let root = List.hd path in
    match Envt.find level path env.env with
    | x ->
      begin if Envt.root_origin level path env.env = M.Unit then record root env;
        x
      end
    | exception Not_found ->
      if Name.Set.mem root env.protected then
        raise Not_found
      else begin
        record root env;
        Module.create ~origin:M.Extern (name path) S.empty
      end
        (* | Not_found -> raise Not_found *)

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

module Make(Envt:envt) = struct
  let minor module_expr str state m =
    let value l v = match str state v with
      | Done _ -> l
      | Halted h -> h :: l in
    let access n m = match Envt.find Module [n] state with
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
      Halted (Minor { access; values; packed } )


  let open_ state path =
    match Envt.find Module path state with
    | x -> Done (Some( D.sg_see x.signature ))
    | exception Not_found -> Halted (Open path)

  let gen_include unbox box i = match unbox i with
    | Halted h -> Halted (box h)
    | Done fdefs ->
      let defs = P.to_defs fdefs in
      Done (Some defs)

  let include_ state module_expr =
    gen_include (module_expr state) (fun i -> Include i)
  let sig_include state module_type = gen_include
      (module_type state) (fun i -> SigInclude i)


  let bind state module_expr lvl {name;expr} =
    match module_expr state expr with
    | Halted h -> Halted ( Bind(lvl, {name; expr = h} ) )
    | Done d ->
      let m = P.to_module name d in
      Done (Some(Def.gen lvl m))

  let bind_rec state module_expr bs =
    let pair x y = x,y in
    let mockup {name;_} =
      {M.origin = Rec; name; args = []; signature = S.empty } in
    let add_mockup defs arg = Envt.add_module defs @@ mockup arg in
    let state' = List.fold_left add_mockup state bs in
    let mapper {name;expr} =
      expr |> module_expr state' |> fmap (pair name) (pair name) in
    let bs = List.map mapper bs in
    let undone (name,defs) = name, (Resolved defs:module_expr) in
    let recombine (name,expr) = {name;expr} in
    match all_done undone bs with
    | Halted bs -> Halted (Bind_rec (List.map recombine bs))
    | Done defs ->
      let defs =
        List.fold_left
          ( fun defs (name,d) -> D.bind (P.to_module name d) defs )
          D.empty defs in
      Done ( Some defs )

  let drop_state = function
    | Done(_state,x) -> Done x
    | Halted _ as h -> h

  let rec module_expr state (me:module_expr) = match me with
    | Opaque _ -> Done P.empty (** todo : add warning *)
    | Ident i ->
      begin match Envt.find Module i state with
        | x -> Done (P.of_module x)
        | exception Not_found -> Halted (Ident i: module_expr)
      end
    | Apply {f;x} ->
      begin match module_expr state f, module_expr state x with
        | Done f, Done _ -> Done (P.drop_arg f)
        | Halted f, Halted x -> Halted (Apply {f;x} )
        | Halted f, Done d -> Halted (Apply {f; x = Resolved d})
        | Done f, Halted x -> Halted (Apply {f = Resolved f ;x} )
      end
    | Fun {arg;body} -> functor_expr (module_expr,fn, demote_str) state [] arg body
    | Str [] -> Done P.empty
    | Str[Defs d] -> Done (P.no_arg d.defined)
    | Resolved d -> Done d
    | Str str -> Work.fmap (fun s -> Str s) P.no_arg @@
      drop_state @@ m2l state str
    | Constraint(me,mt) ->
      constraint_ state me mt

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
      begin match Envt.find Module_type id state with
        | x -> Done (P.of_module x)
        | exception Not_found -> Halted (Ident id: module_type)
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
    | Opaque -> Halted (Opaque:module_type)

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
        | Done d   -> Done (Some{Arg.name=arg.name; signature = P.to_sign d}) in
    match ex_arg with
    | Halted me -> Halted (fn @@ List.fold_left demote {arg=me;body} args )
    | Done arg ->
      let sg = Option.( arg >>| M.of_arg >>| S.create >< S.empty ) in
      let state =  Envt.( state >> sg ) in
      match body_type state body with
      | Done p  -> Done { p with args = arg :: p.args }
      | Halted me ->
        let arg = Option.(
            arg >>| fun arg ->
            { Arg.name = arg.name;
              signature:module_type= Resolved (P.no_arg arg.signature) }
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

  and expr state = function
    | Defs d -> Done (Some d)
    | Open p -> open_ state p
    | Include i -> include_ state module_expr i
    | SigInclude i -> sig_include state module_type i
    | Bind (lvl, b) -> bind state module_expr lvl b
    | Bind_rec bs -> bind_rec state module_expr bs
    | Minor m -> minor module_expr m2l state m
end

module Sg = Make(Envt)
module Tr = Make(Tracing)
