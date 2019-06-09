open M2l
open Mresult

module Y = Summary
module P = Module.Partial

module M = Module
module Arg = M.Arg
module S = Module.Sig
module Faults = Standard_faults
module T = Transforms

open With_deps

let some x = Some x
let ok x = no_deps (Ok x)
let err x = no_deps (Error x)

module Make(Envt:Stage.envt)(Param:Stage.param) = struct

  include Param
  let fault x = Fault.handle policy x
  let raisef f t = fault (Fault.emit f t)


  let find ?edge loc level path env =
    let edge =
      if not epsilon_dependencies then Some Deps.Edge.Normal
      else edge in
    match Envt.find ?edge loc level path env with
    | exception Not_found -> no_deps None
    | {main; deps; msgs} ->
      List.iter fault msgs;
      deps <+> no_deps (Some main)

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


  type level = Module.level = Module | Module_type

  let access (path,_) state x =
    let access n (loc,edge) m =
      find ~edge (path,loc) Module n state >>= function
      | Some _ -> m
      | None ->  m >>| Paths.S.Map.add n (loc,edge) in
    Paths.S.Map.fold access x (no_deps Annot.Access.empty) >>| fun access ->
    if access = Annot.Access.empty then
      Ok ()
    else
      Error access

  let minor (path,_ as loc) module_expr str state m =
    let value l v = str state v >>= function
      | Ok (_,_) ->  l
      | Error h -> l >>| List.cons h in
    let packed l (p: _ Loc.ext) =
      module_expr (path,p.loc) state p.data >>= function
      | Ok _ -> l
      | Error h -> l >>| List.cons (Loc.create p.loc h) in
    let flatten ((a,b),c) = a, b, c in
    ( access loc state m.access
      <*> List.fold_left value (no_deps []) m.values
      <*> List.fold_left packed (no_deps []) m.packed) >>| flatten >>| function
    | Ok (), [], [] -> Ok None
    | Ok (), values, packed ->
      Error { access = Annot.Access.empty; values; packed }
    | Error access, values, packed -> Error { access; values; packed }

  let mt_ident loc level state id =
    find loc level id state >>| function
      | Some M x -> Ok (P.of_module x)
      | Some Namespace _ -> (* FIXME? *) Error id
      | None -> Error id

  let epath loc state path =
    let paths = Paths.Expr.multiples path in
    let r = match paths with
      | a :: q ->
        let resolve dl x =
          mt_ident loc Module state x <*> dl >>| fun (mt, l) -> mt :: l in
        mt_ident loc Module_type state a
        <*> List.fold_left resolve (no_deps []) q
        >>| some
      | [] -> no_deps None in
    r >>| function
    | Some (Ok x, rest) when List.for_all is_ok rest -> Ok x
    | _ -> Error path

  let gen_include loc unbox box i = unbox i >>| function
    | Error h -> Error (box h)
    | Ok fdefs -> Ok (Some (T.gen_include policy loc fdefs))


  let include_ loc state module_expr =
    gen_include loc (module_expr loc state) (fun i -> Include i)
  let sig_include loc state module_type =
    gen_include loc (module_type loc state) (fun i -> SigInclude i)

  let bind state module_expr {name;expr} =
   module_expr state expr >>| function
    | Error h -> Error ( Bind {name; expr = h} )
    | Ok d -> Ok (Some(T.bind_summary Module name d))

  let bind state module_expr (b: M2l.module_expr bind) =
    match b.expr with
    | (Ident p:M2l.module_expr)
    | Constraint(Abstract, Alias p)
      when Envt.is_exterior p state && transparent_aliases ->
        let path = Namespaced.of_path @@ Envt.expand_path p state in
        let m = Module.Alias
            { name = b.name; path; weak=false; phantom = None } in
        let r = Ok ( Some (Y.define [m]) ) in
        no_deps r
    | _ -> bind state module_expr b


  let merge_deps l =
    let merge dl x = dl <*> x >>| fun (l,x) -> x :: l in
    List.fold_left merge (no_deps []) l

  let all_done_with_deps undone l = merge_deps l >>| all_done undone

  let bind_sig state module_type {name;expr} =
    module_type state expr >>| function
    | Error h -> Error ( Bind_sig {name; expr = h} )
    | Ok d -> Ok (Some(T.bind_summary M.Module_type name d))

  let bind_rec state module_expr module_type bs =
    let pair name expr = {name;expr} in
    let triple name me mt = name, me, mt in
    (* first we try to compute the signature of each argument using
       approximative signature *)
    let mockup ({name;_}:_ M2l.bind) = M.md @@ M.mockup name in
    let add_mockup defs arg =
      Envt.extend defs @@ Y.define [mockup arg] in
    let state' = List.fold_left add_mockup state bs in
    let mapper {name;expr} = match expr with
      | Constraint(me,mt) ->
        module_type state' mt >>| fmap (triple name me) (triple name me)
      | _ -> assert false (* recursive module are always constrained *)
    in
    let mts = List.map mapper bs in
    let undone (name, me, defs) = name, me, (Resolved defs:module_type) in
    let recombine (name,me,mt) = {name;expr = Constraint(me,mt) } in
    all_done_with_deps undone mts >>= function
    | Error bs -> err @@ Bind_rec (List.map recombine bs)
    | Ok defs ->
      (* if we did obtain the argument signature, we go on
         and try to resolve the whole recursive binding *)
      let add_arg defs (name, _me, arg) =
        Envt.extend defs
        @@ Y.define [M.M (P.to_module ~origin:Arg name arg)] in
      let state' = List.fold_left add_arg state defs in
      let mapper {name;expr} =
        module_expr state' expr >>| fmap (pair name) (pair name) in
      let bs = List.map mapper bs in
      let undone { name; expr } = { name; expr = (Resolved expr:module_expr) } in
      all_done_with_deps undone bs >>| function
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
      | None -> ok None
      | Some arg ->
        module_type state arg.Arg.signature >>| function
        | Error h -> Error (Some {Arg.name = arg.name; signature = h })
        | Ok d   -> Ok (Some(P.to_arg arg.name d)) in
    ex_arg >>= function
    | Error me -> err (fn @@ List.fold_left demote {arg=me;body} args )
    | Ok arg ->
      let sg =
        Option.( arg >>| (fun m -> Y.define [M.M m] ) >< Y.empty ) in
      let state =  Envt.extend state sg in
      body_type state body >>| function
      | Ok p  -> Ok { p with P.args = arg :: p.P.args }
      | Error me ->
        let arg = Option.(
            arg >>| fun arg ->
            { Arg.name = arg.name;
              signature:module_type= Resolved (P.of_module arg) }
          ) in
        Error (fn @@ List.fold_left demote {arg;body=me} args)


  let rec module_expr loc state (me:module_expr) = match me with
    | Abstract -> ok P.empty
    | Unpacked -> ok P.{ empty with result = Blank; origin = First_class }
    | Val m -> begin
        minor loc module_expr (m2l @@ T.filename loc) state m >>| function
        | Ok _ -> Ok { P.empty with result = Blank; origin = First_class }
        | Error h -> Error (Val h: module_expr)
      end  (* todo : check warning *)
    | Ident i ->
      begin find loc Module i state >>| function
      | Some M x -> Ok (P.of_module x)
      | Some Namespace n -> Ok (P.pseudo_module n)
      | None -> Error (Ident i: module_expr)
      end
    | Apply {f;x} ->
      let f = module_expr loc state f in
      let x = module_expr loc state x in
      begin (f <*> x) >>| function
        | Ok f, Ok _ -> Ok (T.drop_arg policy loc f)
        | Error f, Error x -> Error (Apply {f;x} )
        | Error f, Ok x -> Error (Apply {f; x = Resolved x})
        | Ok f, Error x -> Error (Apply {f = Resolved f ;x} )
      end
    | Fun {arg;body} ->
      functor_expr (module_type loc) (module_expr loc, Build.fn, Build.demote_str)
        state [] arg body
    | Str [] -> ok P.empty
    | Str[{data=Defs d;_ }] -> ok (P.no_arg @@ Y.defined d)
    | Resolved d -> ok d
    | Str str ->
      let str = drop_state <<| m2l (T.filename loc) state str in
      Mresult.fmap (fun s -> Str s) P.no_arg <<| str
    | Constraint(me,mt) ->
      constraint_ loc state me mt
    | Open_me {opens=[]; resolved; expr } ->
      let state = Envt.extend state resolved in
      module_expr loc state expr
    | Open_me {opens=a :: q ; resolved; expr } as me ->
      begin find loc Module a state >>= function
        | None -> err me
        | Some x ->
          let seen = T.open_diverge policy loc x in
          let resolved = Y.( resolved +| seen ) in
          module_expr loc state @@ Open_me {opens = q; resolved; expr }
      end
    | Extension_node n ->
      extension loc state n >>| function
      | Ok _ -> Ok P.empty
      | Error h -> Error(Extension_node h: module_expr)

  and constraint_ loc state me mt =
    let me = module_expr loc state me in
    let mt = module_type loc state mt in
    me <*> mt >>| function
    | Ok _, (Ok _ as r) -> r
    | Ok me, Error mt -> Error (Constraint(Resolved me, mt))
    | Error me, Ok mt -> Error (Constraint(me, Resolved mt) )
    | Error me, Error mt -> Error ( Constraint(me,mt) )

  and module_type loc state = function
    | Sig [] -> ok P.empty
    | Sig [{data=Defs d;_}] -> ok (P.no_arg @@ Y.defined d)
    | Sig s ->
      let s = signature (T.filename loc) state s >>| drop_state in
      Mresult.fmap (fun s -> Sig s) P.no_arg <<| s
    | Resolved d -> ok d
    | Ident id ->
      epath loc state id >>| Mresult.Error.fmap (fun p -> Ident p)
    | Alias i ->
      begin find loc Module i state >>| function
        | Some M x -> Ok (P.of_module x)
        | Some Namespace _ -> (* FIXME: type error *) Error(Alias i)
        | None -> Error (Alias i)
      end
    | With w ->
      begin access loc state w.access >>= function
        | Error access -> err ( With { w with access } )
        | Ok () ->
          begin module_type loc state w.body >>| function
            | Error mt -> Error ( With { w with body = mt } )
            | Ok d -> Ok { d with result = with_deletions w.deletions d.result }
          end
      end
    | Fun {arg;body} ->
      functor_expr (module_type loc)
        (module_type loc, Build.fn_sig, Build.demote_sig)
        state [] arg body
    | Of me -> of_ (module_expr loc state me)
    | Abstract -> ok (P.empty)
    | Extension_node n ->
      begin extension loc state n >>| function
      | Ok _ -> Ok (P.empty)
      | Error n -> Error (Extension_node n)
      end
  and of_ x = Mresult.Error.fmap (fun x -> Of x) <<| x

  and prem2l prev_deps defs filename state = function
    | [] -> prev_deps <+> ok (state, Y.defined defs)
    | a :: q ->
      let deps, x = With_deps.unpack @@ expr filename state a in
      let deps = Deps.( deps + prev_deps ) in
      match x with
      | Ok (Some ext) ->
        (* note: defs ⊂ state *)
        prem2l deps (Y.merge defs ext) filename (Envt.extend state ext) q
      | Ok None -> prem2l deps defs filename state q
      | Error h ->
        deps <+> err @@ snd @@ Normalize.all @@ Loc.nowhere (Defs defs) :: h :: q

  and m2l filename= prem2l Deps.empty Y.empty filename
  and signature filename state  = m2l filename state

  and expr file state e =
    let loc = file, e.loc in
    let reloc = Mresult.Error.fmap @@ Loc.create e.loc in
    reloc <<| match e.data with
    | Defs d -> ok (Some d)
    | Open me ->
      module_expr loc state me >>| fun me ->
      me
      |> Mresult.Ok.fmap (fun x -> T.open_ policy loc x)
      |> Mresult.fmap (fun x -> Open x) some
    | Include i -> include_ loc state module_expr i
    | SigInclude i -> sig_include loc state module_type i
    | Bind b -> bind state (module_expr loc) b
    | Bind_sig b -> bind_sig state (module_type loc) b
    | Bind_rec bs -> bind_rec state (module_expr loc) (module_type loc) bs
    | Minor m ->
      Mresult.Error.fmap (fun m -> Minor m)
      <<| minor loc module_expr (m2l file) state m
    | Extension_node n ->
      Mresult.Error.fmap (fun h -> (Extension_node h: expression))
      <<| extension loc state n
  and extension loc state e =
    if not transparent_extension_nodes then
      ( raisef Faults.extension_ignored (loc,e.name); ok None )
    else
      begin
        let filename = fst loc in
        raisef Faults.extension_traversed (loc,e.name);
        begin let open M2l in
          match e.extension with
          | Module m ->
            begin m2l filename state m >>| function
              | Ok(_,_) -> Ok None
              | Error x -> Error { e with extension=  Module x }
            end
          | Val x ->
            fmap (fun x -> { e with extension=Val x}) (fun _ -> None)
            <<| minor loc module_expr (m2l filename) state x
        end
      end

  type on_going = M2l.t With_deps.t

  let initial x = no_deps x

  let next ~pkg envt input =
    let deps, r = unpack input in
    let deps2, r = unpack (m2l pkg envt r) in
    let deps = Deps.merge deps deps2 in
    match r with
    | Ok (_,x) -> Ok (x, deps)
    | Error x -> Error (deps <+> no_deps x)

  let block x = M2l.Block.m2l (value x)

  let recursive_patching m2l y = With_deps.map m2l begin function
      | { Loc.data = M2l.Defs def; loc } :: q ->
        { Loc.data = M2l.Defs (Summary.merge def y); loc } :: q
      | code ->
        (Loc.nowhere @@ M2l.Defs y) :: code
    end

  let pp ppf x =
    let deps, x = With_deps.unpack x in
    Format.fprintf ppf "@[<2>deps:@ %a@,m2l:@ %a@]" Deps.pp deps M2l.pp x
end
