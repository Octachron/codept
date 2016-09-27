


module type resolver = sig
  type env
  val find: Epath.q -> env -> (Module.signature, Unresolved.t) Either.t option
  val is_root: Name.t -> env -> bool
  val open_module: env -> Module.signature -> env
  val add_root: env -> Name.t -> Module.explicit_signature -> env
  val pp: Format.formatter -> env -> unit
end

module type S = sig

  type env
(*
  val path: Name.set -> env -> Unresolved.t ->
    Name.set * (Module.signature,Unresolved.t) Either.t *)
  val map: Name.set -> env -> Unresolved.map
    -> Name.set * Unresolved.map

  val explicit_signature:
    env -> Module.explicit_signature -> Module.explicit_signature

  val add_root: env -> Name.t -> Module.explicit_signature -> env

end

module PreFilter = struct
  type env = Name.set
  let open_module env _s = env
  let add_root env _name _s = env
  let find q env =
    let name = Epath.prefix @@ snd q in
    if Name.Set.mem name env then
      None
    else
      Some (Either.Left Module.(Sig empty_sig))

  let pp ppf s = Pp.fp ppf "Filter %a" Pp.(clist string) (Name.Set.elements s)
end

module PreFullFilter = struct
  include PreFilter
  let is_root _n _env = true
end

module PreLocalFilter = struct
  include PreFilter
  let is_root _n _env = false
end



module Make(Envt: resolver): S with type env = Envt.env = struct

  type env = Envt.env
  let add_root = Envt.add_root

  module U = Unresolved
  module Md = Module
  open Either
  module D = Name.Set
  let (++) x s = D.add x s

  exception Empty_path

  let update_deps env x deps =
    let name = Epath.prefix x in
    if Envt.is_root name env then
      name ++ deps
    else
      deps

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
        let deps = update_deps env (snd p) deps in
        begin match q with
          | [] -> env, deps , Left s
          | q ->
            let env' = Envt.open_module env Module.( s / del) in
            update_u deps env' q
        end
      | Some (Either.Right u) ->
        let deps = update_deps env (snd p) deps in
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
          let env' = Envt.open_module env s in
          let deps, m' = map deps env' inner in
          env, deps, union m m'
        | deps, Right u ->
          env, deps, U.M.add (U.Extern u) inner m
      end
    | U.Loc (qpath,del) ->
      match Envt.find qpath env with
      | Some (Either.Left s) ->
        let deps = update_deps env (snd qpath) deps in
        (* Vital trick in presence of first-class functor: do not open
           module that have no dependent opens *)
        if Unresolved.( M.cardinal (direct inner) ) > 0 then
          let s = Module.( s / del ) in
          let env' = Envt.open_module env s in
          let deps, m' = map deps env' inner in
          env, deps, union m m'
        else
          env, deps, m
      | Some (Either.Right u) ->
        let u = Unresolved.delete_all del u in
        env, deps, U.M.add (U.Extern u) inner m
      | None ->
        env, deps, U.M.add gpath inner m in
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
    let arg = Option.fmap (module_ env) arg in
    let result = signature env result in
    Fun { arg; result }
  | Alias u ->
    begin match path D.empty env u with
      | _, Left s-> s
      | _, Right u -> Alias u
    end
  | Md.Sig s -> Md.Sig (explicit_signature env s)
and explicit_signature env {Md.s; t; includes;approximation } =
    let s = Md.M.map (module_ env) s in
    let t = Md.M.map (module_ env) t in
    Md.S.fold (include_ env) includes {Md.s;t;includes=Md.S.empty;approximation}
and include_ env u esn =
  let open Md in
  match path D.empty env u with
  | _, Left s ->
    begin match s with
      | Sig s ->
        { s = M.union' esn.s s.s;
          t = M.union' esn.t s.t;
          includes = S.union s.includes esn.includes;
          approximation =esn.approximation
        }
      | Alias u -> { esn with includes = S.add u esn.includes }
      | Fun _ -> raise Error.Including_a_functor
    end
  | _, Right u -> { esn with includes = S.add u esn.includes }


end

type 'env t = (module S with type env = 'env)

module E = Envt
module Envt = Make(Envt.Resolver)
module Precise_filter = Make(PreFullFilter)
module Local_filter = Make(PreLocalFilter)


let envt: E.t t = (module Envt)
let precise_filter: Name.set t = (module Precise_filter)
let local_filter: Name.set t = (module Local_filter)


module Compose(X: S) (Y: S) : S with type env = X.env * Y.env = struct

  type env = X.env * Y.env

  let add_root (xenv, yenv) name s =
    X.add_root xenv name s, Y.add_root yenv name s

  let map deps (xenv,yenv) map =
    let deps, map = X.map deps xenv map in
    Y.map deps yenv map

  let explicit_signature (xenv,yenv) sn =
    sn |> X.explicit_signature xenv |> Y.explicit_signature yenv

end

let (++) (type x y)
    (module X: S with type env = x)
    (module Y: S with type env = y) =
  let module M = Compose(X)(Y) in
  (module M : S with type env = x * y)
