

let access (q,path) env =
  match Module.find path (Envt.get q env) with
  | Some _ -> env
  | None -> Envt.{ env with
                  unresolved = Unresolved.(add_new (loc q path) env.unresolved) }


exception Opening_a_functor

let open_module kind env =
  let union = Module.M.union (fun _key m1 _m2 -> Some m1) in
  let open Module in
  function
  | Alias p ->
    Envt.{ env with unresolved = Unresolved.(down_into (Extern p) env.unresolved) }
  | Fun _ -> raise Opening_a_functor
  | Sig { s; includes } ->
    let env = Envt.{ env with
                     unresolved = S.fold Unresolved.(fun u m ->
                         add_new (Extern u) m) includes env.unresolved
                   } in
    Envt.update kind (union s) env


let open_ kind path env =
  let open_unknown p =
    Envt.{ env with unresolved = Unresolved.down_into p env.unresolved } in
  match Envt.find (kind,path) env with
  | Some (Either.Left m) -> open_module kind env m
  | Some (Either.Right path) -> open_unknown (Unresolved.Extern path)
  | None -> open_unknown (Envt.loc kind path)

let enter_module env = { env with Envt.signature = Module.empty_sig  }

let bind kind env md=
  let add_r m = Module.M.add md.Module.name md m in
  let add_s m = Module.{ m with s = add_r m.s  } in
  let env = Envt.{ env with signature = add_s env.signature } in
  Envt.update kind add_r env

exception Include_functor
let include_ kind env (unresolved,sign) =
  let merge s s' = Module.M.union (fun _k _x y -> Some y) s s' in
  let sign0 = env.Envt.signature in
  match sign with
  | Module.Sig {Module.s;includes} ->
    let env = Envt.update kind (merge s) env in
      { env with
        Envt.signature = Module.{ s = merge sign0.s s;
                      includes = S.union includes sign0.includes
                    };
        unresolved
      }
  | Module.Alias u ->
    let signature = Module.{ sign0 with includes = S.add u sign0.includes } in
    { env with Envt.signature; unresolved }
  | Module.Fun _ -> raise Include_functor

let up sign env = Envt.{ (up env) with signature = sign }

let find_signature kind env path =
  let unresolved = Envt.unresolved env in
  match Envt.find (kind,path) env with
    | Some(Either.Left sign ) -> unresolved, sign
    | Some(Either.Right unk) -> Unresolved.(add_new (Extern unk) unresolved),
                                Module.Alias unk
    | None ->
      Unresolved.(add_new (Envt.loc Epath.Module path) unresolved),
      Module.Alias (Unresolved.alias_with_context
                      env.Envt.unresolved (Epath.Module,path))


let rec refine env u =
  let us = Unresolved.to_list u in
  update env us
and update env = function
  | [] -> env, None
  | a :: q as l->
    match a with
    | Unresolved.Extern u ->
      begin
        match refine env u with
        | env, None -> update env q
        | _ , Some _ as res -> res
      end
    | Unresolved.Loc (p,del) ->
      match Envt.find p env with
      | Some (Either.Left s) ->
        let env' = open_module Epath.Module env s in
        update env' q
      | Some (Either.Right u) ->
        let u = Epath.Set.fold Unresolved.delete del u in
        env, Some (Unresolved.unlist ((Unresolved.Extern u) :: q) )
      | None ->
        env, Some (Unresolved.unlist l)

(*
let rec refine env (Unresolved.Map m) =
  let open Unresolved in
  let update rpath inner m =
    match Envt.find rpath.path env with
    | Some (Either.Left s) ->
      let q, p = rpath.path in
      let env' = open_ q p env in
      let Map m' = refine env' inner in
      Map ( M.union (fun _k _x x' -> Some x') m m' )
    | Some (Either.Right u) ->
      Map ( M.add (Extern u) (refine env inner) m )
    | None ->
      Map ( M.add (Loc rpath) (refine env inner) m ) in
  Map (M.fold update M.empty m)
*)
