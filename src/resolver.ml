

let access (q,path) env =
  match Module.find path (Envt.get q env) with
  | Some _ -> env
  | None -> Envt.{ env with
                  unresolved = Unresolved.(add_new (loc q path) env.unresolved) }


exception Opening_a_functor
exception Including_a_functor

let open_module env =
  let union = Module.M.union (fun _key m1 _m2 -> Some m1) in
  let open Module in
  function
  | Alias p ->
    Envt.{ env with unresolved = Unresolved.(down_into (Extern p) env.unresolved) }
  | Fun _ -> raise Opening_a_functor
  | Sig { s; includes; _ } ->
    let env = Envt.{ env with
                     unresolved = S.fold Unresolved.(fun u m ->
                         add_new (Extern u) m) includes env.unresolved
                   } in
    Envt.update Epath.Module (union s) env


let open_ path env =
  let kind = Epath.Module in
  let open_unknown p =
    Envt.{ env with unresolved = Unresolved.down_into p env.unresolved } in
  match Envt.find (kind,path) env with
  | Some (Either.Left m) -> open_module env m
  | Some (Either.Right path) -> open_unknown (Unresolved.Extern path)
  | None -> open_unknown (Envt.loc kind path)

let enter_module env = { env with Envt.signature = Module.empty_sig  }

let bind env md=
  let add_r m = Module.M.add md.Module.name md m in
  let add_s m = Module.{ m with s = add_r m.s  } in
  let env = Envt.{ env with signature = add_s env.signature } in
  Envt.update md.Module.kind add_r env

exception Include_functor
let include_ kind env (unresolved,sign) =
  let merge s s' = Module.M.union (fun _k _x y -> Some y) s s' in
  let sign0 = env.Envt.signature in
  match sign with
  | Module.Sig {Module.s; t; includes} ->
    let env = Envt.update kind (merge s) env in
      { env with
        Envt.signature = Module.{ s = merge sign0.s s; t = merge sign0.t t;
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

module Refine = struct

  module U = Unresolved
  module Md = Module
  open Either
  module D = Set.Make(struct type t = Epath.t let compare = compare end)
  let (++) x s = D.add x s
  let (+++) s s' = D.union s s'

exception Empty_path

let rec path deps env u =
  let us = Unresolved.to_list u in
  update_u deps env us
and update_u deps env = function
  | [] -> raise Empty_path
  | a :: q as l ->
    match a with
    | Unresolved.Extern u ->
      begin
        match path deps env u with
        | env, deps, Left _ -> update_u deps env q
        | _ , _deps, Right _ as res -> res
      end
    | Unresolved.Loc (p,del) ->
      match Envt.find p env with
      | Some (Left s) ->
        let deps = snd p++deps in
        begin match q with
          | [] -> env, deps , Left s
          | q ->
            let env' = open_module env Module.( s / del) in
            update_u deps env' q
        end
      | Some (Either.Right u) ->
        let u = Epath.Set.fold Unresolved.delete del u in
        env, deps, Right (Unresolved.unlist ((Unresolved.Extern u) :: q) )
      | None ->
        env, deps, Right (Unresolved.unlist l)
let path deps env u =
  let _, deps, res =  path deps env u in
  deps, res


let rec map deps env (Unresolved.Map m0) =
  let union m (U.Map m') = U.M.union (fun _k _x x' -> Some x') m m' in
  let update gpath inner (env,deps,m) =
    match gpath with
    | U.Extern u ->
      begin match path deps env u with
        | deps, Left s ->
          let env' = open_module env s in
          let deps, m' = map deps env' inner in
          env, deps, union m m'
        | deps, Right u ->
          let deps, m' = map deps env inner in
          env, deps, U.M.add (U.Extern u) m' m
      end
    | U.Loc (qpath,del) ->
      match Envt.find qpath env with
      | Some (Either.Left s) ->
        let deps = snd qpath ++ deps in
        let s = Module.( s / del ) in
        let env' = open_module env s in
        let deps, m' = map deps env' inner in
        env, deps, union m m'
      | Some (Either.Right u) ->
        let u = Unresolved.delete_all del u in
        let deps, m' = map deps env inner in
        env, deps, U.M.add (U.Extern u) m' m
      | None ->
        let deps, m' = map deps env inner in
        env, deps, U.M.add gpath m' m in
  let _ ,deps, m = U.M.fold update m0 (env,deps,U.M.empty) in
  deps, U.Map m

let rec module_ env m =
  let signature = signature env m.Md.signature in
  Md.{ m with signature }
and signature env =
  let open Md in
  let open Either in
  function
  | Fun {arg;result} ->
    let arg = module_ env arg in
    let result = signature env result in
    Fun { arg; result }
  | Alias u ->
    begin match path D.empty env u with
      | _, Left s-> s
      | _, Right u -> Alias u
    end
  | Md.Sig {Md.s; t; includes } ->
    let s = Md.M.map (module_ env) s in
    let t = Md.M.map (module_ env) t in
    Sig (Md.S.fold (include_ env) includes {s;t;includes=Md.S.empty})
and include_ env u esn =
  let open Md in
  let union m m' = Md.M.union (fun _k _x y -> Some y) m m' in
  match path D.empty env u with
  | _, Left s ->
    begin match s with
      | Sig s ->
        { s = union esn.s s.s;
          t = union esn.t s.t;
          includes = S.union s.includes esn.includes }
      | Alias u -> { esn with includes = S.add u esn.includes }
      | Fun _ -> raise Including_a_functor
    end
  | _, Right u -> { esn with includes = S.add u esn.includes }


end
