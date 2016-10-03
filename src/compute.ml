open M2l
open Work
open Definitions


module type envt = sig
  type t
  val find: M2l.level -> Npath.t -> t -> module_
  val (>>) : t -> M2l.signature -> t
  val add_module: t -> module_ -> t
end


module Envt = struct
  type t = M2l.signature
  let rec find level path env =
    let proj lvl env = match lvl with
      | Module -> env.modules
      | Module_type -> env.module_types in
    match path with
    | [] -> raise (Invalid_argument "Envt.find cannot find empty path")
    | [a] -> Name.Map.find a @@ proj level env
    | a :: q ->
      let m = Name.Map.find a env.modules in
      find level q m.signature

  let (>>) = Definitions.(+@)
  let add_module = M2l.add_module
end


module Tracing_envt() = struct

  type t = Envt.t
  let deps  = ref Name.Set.empty
  let record n = deps := Name.Set.add n !deps

  let name path = List.hd @@ List.rev path

  let find level path env =
    let origin = List.hd path in
    match Envt.find level path env with
    | x -> x
    | exception Not_found -> record origin;
      { name = name path; alias_of = None; args = []; signature = empty_sig }
  (* | Not_found -> raise Not_found *)

  let (>>) = Definitions.(+@)
  let add_module = M2l.add_module

end



let rec basic = function
  | [] -> []
  | a :: q -> match Reduce.expr a with
    | Done (Some d) -> Defs d :: basic q
    | Halted h -> h :: q
    | Done None -> basic q

module Make(Envt:envt) = struct
  let minor str state m =
    let value l v = match str state v with
      | Done _ -> l
      | Halted h -> h :: l in
    let access n m = match Envt.find Module [n] state with
      | _ -> m
      | exception Not_found -> Name.Set.add n m in
    let values = List.fold_left value [] m.values in
    let access = Name.Set.fold access m.access Name.Set.empty in
    (* to do opaque *)
    if access = Name.Set.empty && values = [] && m.opaques = [] then
      Done None
    else
      Halted (Minor { access; values; opaques = m.opaques } )


  let open_ state path =
    match Envt.find Module path state with
    | x -> Done (Some(mod_def x))
    | exception Not_found -> Halted (Open path)

  let gen_include unbox box i = match unbox i with
    | Halted h -> Halted (box h)
    | Done fdefs ->
      let defs = to_defs fdefs in
      Done (Some defs)

  let include_ state module_expr =
    gen_include (module_expr state) (fun i -> Include i)
  let sig_include state module_type = gen_include
      (module_type state) (fun i -> SigInclude i)


  let bind state module_expr lvl {name;expr} =
    match module_expr state expr with
    | Halted h -> Halted ( Bind(lvl, {name; expr = h} ) )
    | Done d ->
      let m = Definitions.to_module name None d (* lost aliasing? *) in
      Done (Some(Definitions.gen_def lvl m))

  let bind_rec state module_expr bs =
    let pair x y = x,y in
    let mockup {name;_} =
      {alias_of = None; name; args = []; signature = empty_sig } in
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
          ( fun defs (name,d) -> bind_module (to_module name None d) defs )
          empty_defs defs in
      Done ( Some defs )

  let drop_state = function
    | Done(_state,x) -> Done x
    | Halted _ as h -> h

  let rec module_expr state (me:module_expr) = match me with
    | Opaque _ -> Done (empty_full) (** todo : add warning *)
    | Ident i ->
      begin match Envt.find Module i state with
        | x -> Done (of_module x)
        | exception Not_found -> Halted (Ident i: module_expr)
      end
    | Apply {f;x} ->
      begin match module_expr state f, module_expr state x with
        | Done f, Done _ -> Done f
        | Halted f, Halted x -> Halted (Apply {f;x} )
        | Halted f, Done d -> Halted (Apply {f; x = Resolved d})
        | Done _, _ -> assert false
      end
    | Fun {arg;body} -> functor_expr state [] arg body
    | Str [] -> Done empty_full
    | Str[Defs d] -> Done (no_arg d.defined)
    | Resolved d -> Done d
    | Str str -> Work.fmap (fun s -> Str s) no_arg @@
      drop_state @@ m2l state str
    | Constraint(me,mt) ->
      constraint_ state me mt

  and constraint_ state me mt =
    match module_expr state me, module_type state mt with
    |  Done _, (Done _ as r) -> r
    | Done me, Halted mt ->
      Halted (Constraint(Resolved me, mt) )
    | Halted me, Done mt ->
      Halted (Constraint(me, Resolved mt) )
    | Halted me, Halted mt -> Halted ( Constraint(me,mt) )

  and module_type state = function
    | Sig [] -> Done empty_full
    | Sig [Defs d] -> Done (no_arg d.defined)
    | Sig s -> Work.fmap (fun s -> Sig s) no_arg @@
      drop_state @@ signature state s
    | Resolved d -> Done d
    | Ident _ | With _ as mt -> Halted mt
    | Fun _ -> Error.include_functor () (** todo *)
    | Of me -> of_ (module_expr state me)
    | Opaque -> Halted (Opaque:module_type)

  and of_ = function
    | Halted me -> Halted (Of me)
    | Done d -> Done d

  and functor_expr state args arg body =
    let ex_arg =
      match arg with
      | None -> Done None
      | Some arg ->
        match module_type state arg.signature with
        | Halted h -> Halted (Some {name = arg.name; signature = h })
        | Done d   -> Done (Some{name=arg.name; signature = to_sign d}) in
    match ex_arg with
    | Halted me -> Halted (List.fold_left demote_str (Fun {arg=me;body}) args )
    | Done arg ->
      let sg = Option.( arg >>| from_arg >>| to_sig Module >< empty_sig ) in
      let state =  Envt.( state >> sg ) in
      match module_expr state body with
      | Done {args; result} -> Done {args = arg :: args; result }
      | Halted me ->
        let arg = Option.(
            arg >>| fun arg ->
            { name = arg.name;
              signature:module_type= Resolved (no_arg arg.signature) }
          ) in
        Halted (List.fold_left demote_str (Fun {arg;body=me}) args)

  and m2l state = function
    | [] -> Done (state, empty_sig)
    | a :: q ->
      match expr state a with
      | Done (Some defs)  ->
        begin match m2l Envt.( state >> defs.visible ) q with
          | Done (state,sg) ->  Done (state, defs.defined +@ sg)
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
    | Minor m -> minor m2l state m
end

module Sg = Make(Envt)
