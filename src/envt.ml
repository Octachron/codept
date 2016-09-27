type level = Root | Local

type t = {
  modules: Module.t Module.M.t;
  roots: Name.set;
  types: Module.t Module.M.t;
  unresolved: Unresolved.focus;
  signature: Module.explicit_signature }

let get kind env = match kind with
  | Epath.Module -> env.modules
  | Epath.Module_type -> env.types

let update kind f env = match kind with
  | Epath.Module -> {env with modules = f env.modules }
  | Epath.Module_type ->  {env with types = f env.types }

let qualify k path = k, path
let loc k path = Unresolved.Loc (qualify k path, Epath.Set.empty )
let extern u = Unresolved.Extern u

let is_root name env = Name.Set.mem name env.roots

let is_root_path path env = match path with
  | Epath.A name -> is_root name env
  | _ -> false

let find (q,path) env = Module.find path @@ get q env
let empty = {
  modules = Module.M.empty;
  roots = Name.Set.empty;
  types = Module.M.empty;
  unresolved = Unresolved.start; signature = Module.empty_sig }


let unresolved env = env.unresolved
let umap f env = { env with unresolved = f env.unresolved }
let up = umap Unresolved.up

let refocus env env'  =
  let env' = umap (Unresolved.refocus_on @@ unresolved env) env' in
  { env' with modules = env.modules; types = env.types }

let pp ppf env =
  let ur = env.unresolved.Unresolved.map in
  Format.fprintf ppf "Unresolved map:@[%a@]@;
  Signature:@[%a@]@."
    Unresolved.pp ur
    Module.pp_explicit env.signature

module Resolver = struct

  type env = t
  let is_root = is_root
  let find = find

  let access (q,path) env =
    match Module.find path (get q env) with
    | Some _ -> env
    | None -> { env with
                unresolved = Unresolved.(add_new (loc q path) env.unresolved) }


  let union s = Module.M.union (fun _k _x y -> Some y) s

  let warn_on_open = function
    | Module.Approximation.First_class_module ->
      Warning.log "A packed module signature (that was subsequently \
                   opened) could not be inferred. @;Subsequent dependencies may \
                   be artefacts."
  let print_warnings warn = function
    | Module.Approximation.Wrong msgs -> List.iter warn msgs
    | _ -> ()

  let open_sig env {Module.s;t;includes;approximation} =
    let open Module in
    print_warnings warn_on_open approximation;
    let env = { env with
                unresolved = S.fold Unresolved.(fun u m ->
                    add_new (Extern u) m) includes env.unresolved
              } in
    env
    |> update Epath.Module (union s)
    |> update Epath.Module_type (union t)


  let open_module env =
    let open Module in
    function
    | Alias p ->
      { env with unresolved = Unresolved.(down_into (Extern p) env.unresolved) }
    | Fun _ as m ->
      raise @@ Error.Opening_a_functor (Format.asprintf "%a" Module.pp_signature m)
    | Sig sn -> open_sig env sn


  let warn_on_new_root name = function
    | Module.Approximation.First_class_module ->
      Warning.log "A packed module signature (that was subsequently \
                   included) could not be inferred. Consequently, the inferred \
                   signature for root module %s is erroneous." name

  let print_root_warnings name warn =
    let open Module.Approximation in
    function
    | Wrong msgs -> List.iter (warn name) msgs
    | Inexact ->
      Warning.log "The inferred signature for root module %s might be wrong. \
                   Some of the computed signature items might be erroneous \
                   artefacts." name
    | Exact -> ()


  let add_root env name esn =
    print_root_warnings name warn_on_new_root esn.Module.approximation;
    let md = {Module.name; kind = Epath.Module; signature = Module.Sig esn} in
    let roots = Name.Set.add name env.roots in
    { env with modules = Module.( md |+> env.modules); roots }


  let open_ path env =
    let kind = Epath.Module in
    let open_unknown p =
      { env with unresolved = Unresolved.down_into p env.unresolved } in
    match find (kind,path) env with
    | Some (Either.Left m) -> open_module env m
    | Some (Either.Right path) -> open_unknown (Unresolved.Extern path)
    | None -> open_unknown (loc kind path)

  let enter_module env = { env with signature = Module.empty_sig  }

  let bind_translucid env md =
    update md.Module.kind (Module.M.add md.Module.name md) env

  let bind env md=
    let add_r m = Module.M.add md.Module.name md m in
    let add_s m = Module.{ m with s = add_r m.s  } in
    let env = { env with signature = add_s env.signature } in
    update md.Module.kind add_r env

  exception Include_functor

  let include_ kind env (unresolved,sign) =
    let sign0 = env.signature in
    match sign with
    | Module.Sig {Module.s; t; includes; approximation} ->
      let env = update kind (union s) env in
      let approximation =
        Module.(Approximation.min approximation sign0.approximation) in
      { env with
        signature = Module.{ s = union sign0.s s; t = union sign0.t t;
                             includes = S.union includes sign0.includes;
                             approximation
                           };
        unresolved
      }
    | Module.Alias u ->
      let signature = Module.{ sign0 with includes = S.add u sign0.includes } in
      { env with signature; unresolved }
    | Module.Fun _ -> raise Include_functor

  let up sign env = { (up env) with signature = sign }

  let find_signature kind env path =
    let unresolved = unresolved env in
    match find (kind,path) env with
    | Some(Either.Left sign ) -> unresolved, sign
    | Some(Either.Right unk) -> Unresolved.(add_new (Extern unk) unresolved),
                                Module.Alias unk
    | None ->
      Unresolved.(add_new (loc Epath.Module path) unresolved),
      Module.Alias (Unresolved.alias_with_context
                      env.unresolved (Epath.Module,path))

let pp = pp

end
