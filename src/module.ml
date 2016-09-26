module M = Name.Map

module S = struct
  include Set.Make(struct type t = Unresolved.t let compare = compare end)
  let singleton x = add x empty
  let map f s = fold (fun x s -> add (f x) s) s empty
end

type t = {name: Name.t; kind:Epath.kind; signature:signature }
and signature =
  | Alias of Unresolved.t
  | Sig of explicit_signature
  | Fun of fn
and explicit_signature = { s: t M.t; t: t M.t; includes:S.t }
and fn = {arg: t option; result: signature }

let (|+>) me m = M.add me.name me m
let singleton m = M.singleton m.name m

let pp_name ppf m =
  Pp.fp ppf "module %s%s"
    (match m.kind with Epath.Module ->""| Epath.Module_type -> "type ")
    m.name

let rec pp ppf s = let open Pp in
  fp ppf "@[<hov2> %a:@,%a@]" pp_name s pp_signature s.signature
and pp_signature ppf = function
  | Alias u -> Pp.fp ppf "@[?%a@]" Unresolved.pp_u u
  | Fun {arg;result} -> Pp.fp ppf "@[<hov2> functor(%a)@,@ ->@ @,%a@]"
                          (Pp.opt pp) arg pp_signature result
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

let empty_sig = { s = M.empty; t = M.empty; includes = S.empty }
let create_sig ?(includes=S.empty) ?(t=M.empty) m = Sig { s = m; t; includes }

let rec create_along path nm =
  match path with
  | [] -> nm
  | a :: q -> { name = a;
                kind = Epath.Module;
                signature = Sig { s = create_along q nm |+> M.empty;
                                  t = M.empty;
                                  includes = S.empty}
                }


let rec delete path m =
  { m with signature = delete_sig path m.signature }
and delete_sig path sgn =
  match path with
  | [] -> sgn
  | a::q ->
    match sgn with
    | Alias u -> Alias (Unresolved.delete (Epath.from_list path) u)
    | Fun _ -> Error.signature_expected ()
    | Sig sg ->
      match M.find a sg.s with
      | m' -> if q = [] then
          Sig { sg with s = M.remove a sg.s}
        else
          Sig { sg with s = M.add a (delete q m') sg.s}
      | exception Not_found ->
        let update u =  Unresolved.delete (Epath.from_list path) u in
        let includes = S.map update sg.includes in
        Sig  { sg with includes }

let delete path m =
  delete (Epath.concrete path) m
let delete_sig path s =
  delete_sig (Epath.concrete path) s


let (/) m set = Epath.Set.fold (fun path m-> delete_sig path m) set m

let replace path ~inside ~signature =
  let name, path = Epath.split path in
  let path = Epath.concrete path in
  let rec replace signature inside = function
    | [] ->
      let inner = {signature; kind = Epath.Module; name} in
      begin match inside.signature with
        | Alias u ->
          let includes = S.singleton @@ Unresolved.delete (Epath.A name) u in
          { inside with signature = create_sig ~includes @@ singleton inner }
        | Fun _ -> Error.signature_expected ()
        | Sig s ->
          let includes = S.map (Unresolved.delete @@ Epath.A name) s.includes in
          let signature = Sig { s = inner |+> s.s; includes; t = s.t } in
          { inside with signature }
      end
    | a :: q as p -> match inside.signature with
      | Fun _ -> Error.signature_expected ()
      | Alias u ->
        let sn = Sig { s=M.empty; includes = S.singleton u; t = M.empty} in
        replace signature { inside with signature = sn} p
      | Sig sg ->
        match M.find a sg.s with
        | m' ->
          { inside with signature =
                            Sig { sg with s = replace signature m' q |+> sg.s } }
        | exception Not_found ->
          if S.cardinal sg.includes = 0 then
              Error.module_type_error a
          else
            let update u =  Unresolved.delete (Epath.from_list path) u in
            let includes = S.map update sg.includes in
            let inner = create_along p {name; kind = Epath.Module; signature} in
            { inside with signature = Sig
                              {s = inner |+> M.empty; includes; t = M.empty } }
  in
  replace signature inside path

let rec ellide path path' =
  let open Epath in
  match path, path' with
  | T, _ -> path'
  | A m , A n  -> if m = n then T else A n
  | S(p,m), S(p',n) ->
    let meet = ellide p p' in
    if meet = T && m = n then T
    else meet / n
  | F _, F f -> F f
  | _, _ -> path'

  (*
  let rec substitute name path =
    function
    | Alias u -> Alias Unresolved.(Epath.substitute ~name ~sub:path $ u)
    | Sig {s;includes} -> Sig {
        s = M.map (fun md ->
            { md with signature = substitute name path md.signature } )
            s;
        includes = S.map
            Unresolved.(fun u ->  Epath.substitute ~name ~sub:path $ u)
            includes
      }
    | Fun {arg; result} -> Fun { arg; result = substitute name path result }
*)


let (>>) x left = Either.map left (fun x -> Either.Right x) x


let rec find env path m =
  let open Epath in
  match path with
  | T -> raise Not_found
  | A n -> Either.Left (M.find n m).signature
  | F fn ->
    find_functor env fn m
  | S(p,n) ->
    let m = find env p m in
    m >> (find_mod env @@ A n)
and find_functor env fn0 m =
  let fn = find env fn0 m in
  fn >> function
  | Sig _ -> raise @@ Error.Functor_expected
      (Format.asprintf "%a" Epath.pp fn0)
  | Alias unkn -> Either.Right unkn (*??*)
  | Fun fn -> Either.Left fn.result
and find_mod env path = function
  | Sig { s;_ } -> find env path s
  | Alias u ->
    (* u.path *)
    Either.Right Unresolved.( u // path ) (* todo ?? *)
  | Fun _ -> raise Error.Functor_not_expected
and apply _env fn _sign  = fn.result

let find_exn = find

let find path env =
  try Some (find env path env) with
  | Not_found -> None
