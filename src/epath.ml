
open Common_types

type t =
  | T
  | A of name
  | S of t * name
  | F of t
  (** functor argument are forgotten: they do not influence module name *)

let split = function
  | T  -> raise (Invalid_argument "Splitting an empty path")
  | F _ -> raise (Invalid_argument "Splitting a functor application")
  | A n -> n, T
  | S(p,a) -> a, p

let rev_concrete p =
  let rec concretize l = function
    | T -> l
    | A a -> a :: l
    | F _ -> raise Functor_not_expected
    | S(p,s) -> concretize (s::l) p in
  concretize [] p

let concrete p = List.rev @@ rev_concrete p

let from_list l =
  let rec rebuild =
    function
    | [] -> T
    | [a] -> A a
    | a :: q -> S(rebuild q, a)
  in rebuild @@ List.rev l

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

let is_prefix prefix path =
  let prefix, path = concrete prefix, concrete path in
  let rec is_prefix prefix path =
    match prefix, path with
    | [], _ -> true
    | _ , [] -> false
    | a :: q, b:: q' -> a = b && is_prefix q q' in
  is_prefix prefix path

let cut_prefix prefix path =
  let prefix, path = concrete prefix, concrete path in
  let rec cut_prefix prefix path =
    match prefix, path with
    | [], _ -> path
    |  _ , [] -> []
    | a :: q, b:: q' ->
      if a = b then
        cut_prefix q q'
      else path in
  from_list @@ cut_prefix prefix path


type kind = Module_type | Module
type q = kind * t
let kind = fst
let pth =snd

let pp_q ppf (k,p) = match k with
  | Module -> pp ppf p
  | Module_type -> Pp.fp ppf "'%a" pp p

module Set = Set.Make(struct type nonrec t = t let compare = compare end )
type set = Set.t
