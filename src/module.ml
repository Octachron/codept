module M = Name.Map

module S = struct
  include Set.Make(struct type t = Unresolved.t let compare = compare end)
  let singleton x = add x empty
  let map f s = fold (fun x s -> add (f x) s) s empty
end

module Approximation = struct
  type explanation = First_class_module
  let pp ppf = function
    | First_class_module -> Pp.fp ppf ""
  type level = Exact | Inexact | Wrong of explanation list

  let min x y = match x, y with
    | Exact, Exact -> Exact
    | Exact, Inexact | Inexact, Exact | Inexact, Inexact -> Inexact
    | Wrong l, Wrong l' -> Wrong (l @ l')
    | Wrong _ as w , (Exact|Inexact) | (Exact|Inexact), (Wrong _ as w) -> w

  let pp_symb ppf = function
    | Exact -> Pp.fp ppf "="
    | Inexact -> Pp.fp ppf "≈"
    | Wrong _ -> Pp.fp ppf "≠"

end

let exact = Approximation.Exact
let warn_module = Approximation.(Wrong [First_class_module])


type t = {
  name: Name.t;
  kind:Epath.kind;
  signature:signature }
and signature =
  | Alias of Unresolved.t
  | Sig of explicit_signature
  | Fun of fn
and explicit_signature = {
  approximation: Approximation.level;
  s: t M.t;
  t: t M.t;
  includes:S.t }
and fn = {arg: t option; result: signature }

let rec sig_level = function
  | Fun { arg=_ ; result } ->
    sig_level result
  | Alias _ -> Approximation.Exact
  | Sig esn -> esn.approximation

let exsig_level ~s ~t =
  let check _k m level = Approximation.min level @@ sig_level m.signature in
  Approximation.Exact
  |> M.fold check s
  |> M.fold check t


let exsig  ?(t=M.empty) ?approx ?(includes=S.empty) s =
  let approximation= match approx with
    | Some x -> x
    | None -> exsig_level ~s ~t in
  { s; t; includes; approximation}


let create ~name ~kind signature =
  {name;kind;signature}



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
  | Sig s -> Pp.fp ppf "sig(%a)@, @[<hov2>%a@]@, end"
               Approximation.pp_symb s.approximation
               pp_explicit s
and pp_explicit ppf ex =
  let submod = Pp.list @@ fun ppf (_,x) -> pp ppf x in
  if S.cardinal ex.includes > 0 then
    Pp.fp ppf "@[<hov2>%a@]@; include?@[[%a]@]"
      submod
      (M.bindings ex.s @ M.bindings ex.t)
      (Pp.list Unresolved.pp_u)
      (S.elements ex.includes)
  else
    Pp.fp ppf "@[<hov2>%a@]"
      submod
      (M.bindings ex.s)

let empty_sig = exsig M.empty

let first_class_approx = Sig { empty_sig with approximation = warn_module }

let create_sig ?approx ?includes ?t m =
  Sig (exsig ?approx ?includes ?t m)



let rec create_along approx path nm =
  match path with
  | [] -> nm
  | a :: q -> { name = a;
                kind = Epath.Module;
                signature = create_sig ~approx (create_along approx q nm |+> M.empty)
                }
let create_along path nm =
  create_along (sig_level nm.signature) path nm

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
      let inner =
        create ~name ~kind:Epath.Module signature in
      begin match inside.signature with
        | Alias u ->
          let includes = S.singleton @@ Unresolved.delete (Epath.A name) u in
          { inside with signature = create_sig ~includes @@ singleton inner }
        | Fun _ -> Error.signature_expected ()
        | Sig s ->
          let includes = S.map (Unresolved.delete @@ Epath.A name) s.includes in
          let signature =
            create_sig ~approx:s.approximation ~t:s.t ~includes (inner |+> s.s) in
          { inside with signature }
      end
    | a :: q as p -> match inside.signature with
      | Fun _ -> Error.signature_expected ()
      | Alias u ->
        let sn = create_sig ~includes:(S.singleton u) M.empty in
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
            let inner = create_along p @@
              create ~name ~kind:Epath.Module signature in
            let signature = create_sig ~approx:exact ~includes (inner |+> M.empty) in
            { inside with signature } in
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
