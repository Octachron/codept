
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
    | F of t
    (** functor argument are forgotten: they do not influence module name *)

  let rec pp ppf =
    let p fmt = Format.fprintf ppf fmt in
    function
    | T -> p "T"
    | A name -> p"%s" name
    | S(h,n) -> p "%a.%s" pp h n
    | F fn -> p "%a(…)" pp fn

  let (/) p n= match p, n with
    | T, n -> A n
    | p, n  -> S(p,n)

  let rec (//) p = function
    | T -> p
    | A n -> p / n
    | S (p',n)  -> (p // p') / n
    | F fn -> F (p//fn)


  let (%) fn _arg = F fn

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
    | F fn ->
      let t, fn = subst fn in
      t , F fn
  in
  snd @@ subst path

  type kind = Module_type | Module
  type q = kind * t
  let kind = fst
  let pth =snd

  let pp_q ppf (k,p) = match k with
    | Module -> pp ppf p
    | Module_type -> Pp.fp ppf "'%a" pp p

end


module Unresolved = struct


  type u = {path:Path.q; env:rpath list}
  and rpath = Loc of Path.q | Extern of u

  let ($) f u =
    let (k,p) = u.path in
    { u with path = k, f p }
  let to_list u = List.rev @@ (Loc u.path) :: u.env


  let rec pp_rpath ppf = function
    | Loc p ->  Path.pp_q ppf p
    | Extern ext -> pp_u ppf ext
  and pp_u ppf u = Pp.clist pp_rpath ppf (to_list u)


  module M = struct
    include Map.Make(struct type t = rpath let compare = compare end)
    let (|+>) (k,x) m = add k x m
    let find_opt k m = try Some(find k m) with Not_found -> None
  end

  type map = Map of map M.t
  let empty = Map M.empty

  type update = (map M.t -> map M.t) -> map M.t -> map M.t
  type focus = { update: update; prev:update list; env:rpath list; map:map }

  let start = { update = (@@); prev = []; env = []; map = empty }

  let top foc =
    { foc with update=(@@); prev = []; env = []  }

  let m { map = Map m; _ } = m
  let update_map foc f = { foc with map = Map (foc.update f @@ m foc) }

  let rec pp ppf (Map m)=
    let p fmt = Format.fprintf ppf fmt in
    if m = M.empty then ()
    else begin
      p "@[<hov2>[";
      M.iter (fun k x ->
          p "%a? @,%a" pp_rpath k pp x
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
    { map = foc.map;
      update;
      prev = foc.update :: foc.prev;
      env = x :: foc.env
    }

  let up foc = match foc.prev with
    | [] -> foc
    | update::prev -> {
        map = foc.map;
        update;
        prev;
        env = List.tl foc.env
      }

  let down_into x foc = foc |> add_new x |> down x

  let move_to path foc =
    List.fold_left (fun foc u -> down u foc) (top foc) path

  let refocus_on foc foc' =
    { foc with map = foc'.map }

  let alias_with_context foc path = {path; env = foc.env }

end




module Module = struct
  module M = struct
    include Map.Make(struct type t = name let compare = compare end)
    let (|+>) (k,x) m = add k x m
    let find_opt k m = try Some(find k m) with Not_found -> None
  end

  module S = struct
    include Set.Make(struct type t = Unresolved.u let compare = compare end)
    let map f s = fold (fun x s -> add (f x) s) s empty
  end


  type t = {name:name; signature:signature }
  and signature =
    | Alias of Unresolved.u
    | Sig of explicit_signature
    | Fun of fn
  and explicit_signature = { s: t M.t; includes:S.t }
  and fn = {arg: t; result: signature }

  let rec pp ppf s = let open Pp in
    fp ppf "@[<hov2> module %s:@,%a@]" s.name pp_signature s.signature
  and pp_signature ppf = function
    | Alias u -> Pp.fp ppf "@[?%a@]" Unresolved.pp_u u
    | Fun {arg;result} -> Pp.fp ppf "@[<hov2> functor(%a)@,@ ->@ @,%a@ end]"
                            pp arg pp_signature result
    | Sig s -> Pp.fp ppf "sig@, @[<hov2>%a@]@, end"
                 pp_explicit s
  and pp_explicit ppf ex =
    let submod = Pp.list @@ fun ppf (_,x) -> pp ppf x in
    if S.cardinal ex.includes > 0 then
      Pp.fp ppf "@[<hov2>%a@]@; include?[%a]@]"
        submod
        (M.bindings ex.s)
        (Pp.list Unresolved.pp_u)
        (S.elements ex.includes)
    else
      Pp.fp ppf "@[<hov2>%a@]@]"
        submod
        (M.bindings ex.s)

  let empty_sig = { s = M.empty; includes = S.empty }

  let rec ellide path path' =
    let open Path in
    match path, path' with
    | T, _ -> path'
    | A m , A n  -> if m = n then T else A n
    | S(p,m), S(p',n) ->
      let meet = ellide p p' in
      if meet = T && m = n then T
      else meet / n
    | F _, F f -> F f
    | _, _ -> path'

  let rec substitute name path =
    function
    | Alias u -> Alias Unresolved.(Path.substitute ~name ~sub:path $ u)
    | Sig {s;includes} -> Sig {
        s = M.map (fun md ->
            { md with signature = substitute name path md.signature } )
            s;
        includes = S.map
            Unresolved.(fun u ->  Path.substitute ~name ~sub:path $ u)
            includes
      }
    | Fun {arg; result} -> Fun { arg; result = substitute name path result }

  exception Functor_expected
  exception Functor_not_expected

  let (>>) x left = Either.map left (fun x -> Either.Right x) x


  let rec find env path m =
    let open Path in
    match path with
    | T -> raise Not_found
    | A n -> Either.Left (M.find n m).signature
    | F fn ->
      find_functor env fn m
    | S(p,n) ->
      let m = find env p m in
      m >> (find_mod env @@ A n)
  and find_functor env fn m =
    let fn = find env fn m in
    fn >> function
    | Sig _ -> raise Functor_expected
    | Alias unkn -> Either.Right unkn (*??*)
    | Fun _ as f-> Either.Left f
  and find_mod env path = function
    | Sig { s;_ } -> find env path s
    | Alias u ->  Either.Right u (* todo ?? *)
    | Fun _ -> raise Functor_not_expected
  and apply _env fn _sign  = Fun fn

  let find_exn = find

  let find path env =
    try Some (find env path env) with
    | Not_found -> None


end



module Env = struct

  type t = {
    modules: Module.t Module.M.t;
    types: Module.t Module.M.t;
    unresolved: Unresolved.focus;
    signature: Module.explicit_signature }

  let get kind env = match kind with
    | Path.Module -> env.modules
    | Path.Module_type -> env.types

  let update kind f env = match kind with
    | Path.Module -> {env with modules = f env.modules }
    | Path.Module_type ->  {env with types = f env.types }

  let qualify k path = k, path
  let loc k path = Unresolved.Loc (qualify k path)
  let extern u = Unresolved.Extern u


  let find (q,path) env = Module.find path @@ get q env
  let empty = {
    modules = Module.M.empty;
    types = Module.M.empty;
    unresolved = Unresolved.start; signature = Module.empty_sig }


  let unresolved env = env.unresolved
  let umap f env = { env with unresolved = f env.unresolved }
  let up = umap Unresolved.up

  let refocus env env'  =
    let env' = umap (Unresolved.refocus_on @@ unresolved env) env' in
    { env' with modules = env.modules; types = env.types }

end


let access (q,path) env =
  match Module.find path (Env.get q env) with
  | Some _ -> env
  | None -> Env.{ env with
                  unresolved = Unresolved.(add_new (loc q path) env.unresolved) }


exception Opening_a_functor

let open_ kind path env =
  let open_unknown p =
    Env.{ env with unresolved = Unresolved.down_into p env.unresolved } in
  let union = Module.M.union (fun _key m1 _m2 -> Some m1) in
  let open_m =
    let open Module in
    function
    | Alias p ->
      Env.{ env with unresolved = Unresolved.(down_into (Extern p) env.unresolved) }
    | Fun _ -> raise Opening_a_functor
    | Sig { s; includes } ->
      let env = Env.{ env with
                      unresolved = S.fold Unresolved.(fun u m ->
                          add_new (Extern u) m) includes env.unresolved
                    } in
      Env.update kind (union s) env
  in
  match Env.find (kind,path) env with
  | Some (Either.Left m) -> open_m m
  | Some (Either.Right path) -> open_unknown (Unresolved.Extern path)
  | None -> open_unknown (Env.loc kind path)

let enter_module env = { env with Env.signature = Module.empty_sig  }

let bind kind env md=
  let add_r m = Module.M.add md.Module.name md m in
  let add_s m = Module.{ m with s = add_r m.s  } in
  let env = Env.{ env with signature = add_s env.signature } in
  Env.update kind add_r env

let up sign env = Env.{ (up env) with signature = sign }
