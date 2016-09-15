
type name = string

module Pp = struct
  let fp = Format.fprintf
  let rec list pp ppf =
    function
    | a :: ( _ :: _ as q ) -> fp ppf "%a; %a" pp a (list pp) q
    | [a] -> fp ppf "%a" pp a
    | [] -> ()

  let blist pp ppf = fp ppf "[%a]" (list pp)
  let clist pp ppf = fp ppf "{%a}" (list pp)
end

module Either = struct
  type ('a,'b) t = Left of 'a | Right of 'b
  let map left right = function
    | Left l -> left l
    | Right r -> right r
end

module Path = struct
  type t =
    | T
    | A of name
    | S of t * name
    | F of {fn:t; arg:t}

  let rec pp ppf =
    let p fmt = Format.fprintf ppf fmt in
    function
    | T -> p "T"
    | A name -> p"%s" name
    | S(h,n) -> p "%a.%s" pp h n
    | F {fn;arg} -> p "%a(%a)" pp fn pp arg

  let (/) p n= match p, n with
    | T, n -> A n
    | p, n  -> S(p,n)

  let rec (//) p = function
    | T -> p
    | A n -> p / n
    | S (p',n)  -> (p // p') / n
    | F {fn; arg} -> F { fn=p//fn; arg}


  let (%) fn arg = F{fn;arg}

  let substitute ~name ~sub path =
  let rec subst path = match path with
    | S( p, x ) ->
      let t, p' = subst p in
      if t then true, S(p',x)
      else if x = name then true, p // sub
      else false, path
    | T -> false, T
    | A m -> if name = m then true, sub
      else false, path
    | F {fn;arg} ->
      let t, fn = subst fn in
      let t', arg = subst arg in
      t || t', F {fn;arg}
  in
  snd @@ subst path

  type u = t list
  type g = Exact of t | Unknown of u
end

module Module = struct
  module M = struct
    include Map.Make(struct type t = name let compare = compare end)
    let (|+>) (k,x) m = add k x m
    let find_opt k m = try Some(find k m) with Not_found -> None
  end

  type t = {name:name; signature:signature }
  and signature =
    | Alias of Path.g
    | Sig of t M.t
    | Fun of { arg: t; result: signature }

  let rec ellide path path' =
    let open Path in
    match path, path' with
    | T, _ -> path'
    | A m , A n  -> if m = n then T else A n
    | S(p,m), S(p',n) ->
      let meet = ellide p p' in
      if meet = T && m = n then T
      else meet / n
    | F {fn;arg}, F f -> if fn = f.fn && arg = f.arg then T else F f
    | _, _ -> path'

  let rec substitute name path =
    function
    | Alias (Path.Exact p) -> Alias Path.(Exact (substitute ~name ~sub:path p))
    | Alias _unknown as a -> a
    | Sig s -> Sig (
        M.map (fun md ->
            { md with signature = substitute name path md.signature } )
          s
      )
    | Fun {arg; result} -> Fun { arg; result = substitute name path result }

  let apply (arg,signature) path =
    substitute arg.name path signature


  exception Functor_expected
  exception Functor_not_expected

  let (>>) x left = Either.map left (fun x -> Either.Right x) x


  let rec find env path m =
    let open Path in
    match path with
    | T -> raise Not_found
    | A n -> Either.Left (M.find n m).signature
    | F {fn;arg} ->
      find_functor env fn arg m
    | S(p,n) ->
      let m = find env p m in
      m >> (find_mod env @@ A n)
  and find_functor env fn arg m =
    let fn = find env fn m in
    fn >> function
    | Sig _ -> raise Functor_expected
    | Alias (Path.Exact fn) ->  find_functor env fn arg env
    | Alias (Path.Unknown unkn) -> Either.Right unkn
    | Fun f -> Either.Left (apply (f.arg,f.result) arg)
  and find_mod env path = function
    | Sig s -> find env path s
    | Alias (Path.Exact m') -> find env m' env >> find_mod env path
    | Alias (Path.Unknown u) ->  Either.Right u (* todo *)
    | Fun _ -> raise Functor_not_expected

  let find path env =
    try Some (find env path env) with
    | Not_found -> None

end


module Unresolved = struct

  type rpath = Loc of Path.t | Extern of Path.t list
  let pp_rpath ppf = function
    | Loc p ->  Path.pp ppf p
    | Extern ext -> (Pp.clist Path.pp) ppf ext

  module M = struct
    include Map.Make(struct type t = rpath let compare = compare end)
    let (|+>) (k,x) m = add k x m
    let find_opt k m = try Some(find k m) with Not_found -> None
  end

  type map = Map of map M.t
  let empty = Map M.empty

  type update = (map M.t -> map M.t) -> map M.t -> map M.t
  type focus = { update: update; prev:update list; map:map }

  let start = { update = (@@); prev = []; map = empty }

  let top foc =
    { foc with update=(@@); prev = [] }

  let m { map = Map m; _ } = m
  let update_map foc f = { foc with map = Map (foc.update f @@ m foc) }

  let rec pp ppf (Map m)=
    let p fmt = Format.fprintf ppf fmt in
    if m = M.empty then ()
    else begin
      p "@[[";
      M.iter (fun k x ->
          p "%a? %a" pp_rpath k pp x
        ) m;
      p "]@]@,";
    end

  let (|=) = update_map

  let add_new x foc = foc |= fun m ->
      match M.find_opt x m with
      | None ->  M.add x empty m
      | Some _ -> m

  let down x foc =
    let update f =
      let f' m =
        let Map m' = M.find x m in
        M.add x (Map(f m')) m in
      foc.update f'
    in
    { foc with update; prev = foc.update :: foc.prev }

  let up foc = match foc.prev with
    | [] -> foc
    | update::prev -> { foc with update; prev }

  let down_into x foc = foc |> add_new x |> down x

  let move_to path foc =
    List.fold_left (fun foc u -> down u foc) (top foc) path

end


module Env = struct
  type t = {
    resolved: Module.t Module.M.t;
    unresolved: Unresolved.focus;
    signature: Module.t list }
  let find path env = Module.find path env.resolved
  let empty = {
    resolved = Module.M.empty;
    unresolved = Unresolved.start; signature = [] }
end

let access path env =
  match Module.find path env.Env.resolved with
  | Some _ -> env
  | None -> Env.{ env with
                  unresolved = Unresolved.(add_new (Loc path) env.unresolved) }

exception Opening_a_functor

let rec open_ path env =
  let open_unknown p =
    Env.{ env with unresolved = Unresolved.down_into p env.unresolved } in
  let union = Module.M.union (fun _key m1 _m2 -> Some m1) in
  let open_m =
    let open Module in
    function
    | Alias (Path.Exact p) -> open_ p env
    | Alias (Path.Unknown p) ->
      Env.{ env with unresolved = Unresolved.(down_into (Extern p) env.unresolved) }
    | Fun _ -> raise Opening_a_functor
    | Sig s ->
      Env.{ env with resolved = union s env.resolved }
  in
  match Env.find path env with
  | Some (Either.Left m) -> open_m m
  | Some (Either.Right path) -> open_unknown (Unresolved.Extern path)
  | None -> open_unknown (Unresolved.Loc path)

let enter_module env = { env with Env.signature = [] }

let bind env md=
  Env.{ env with
    resolved = Module.M.add md.Module.name md env.resolved;
    signature = md :: env.signature }
