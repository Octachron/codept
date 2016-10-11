type t =
  | T
  | A of Name.t
  | S of t * Name.t
  | F of {f:t; x:t}

let split = function
  | T  -> raise (Invalid_argument "Splitting an empty path")
  | F _ -> raise (Invalid_argument "Splitting a functor application")
  | A n -> n, T
  | S(p,a) -> a, p

let concrete p: Npath.t =
  let rec concretize l = function
    | T -> l
    | A a -> a :: l
    | F _ -> raise Error.Functor_not_expected
    | S(p,s) -> concretize (s::l) p in
  concretize [] p

let concrete_with_f p: Npath.t =
  let rec concretize l = function
    | T -> l
    | A a -> a :: l
    | F {f;_} -> concretize l f
    | S(p,s) -> concretize (s::l) p in
  concretize [] p


let multiples p : Npath.t list =
  let rec concretize stack l = function
    | T -> l :: stack
    | A a -> (a :: l) :: stack
    | F {f;x} -> concretize (concretize stack [] x) l f
    | S(p,s) -> concretize stack (s::l) p in
  concretize [] [] p

let rev_concrete p = List.rev @@ concrete p

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
  | F {f;x} -> p "%a(%a)" pp f pp x

let (/) p n= match p, n with
  | T, n -> A n
  | p, n  -> S(p,n)

let rec (//) p = function
  | T -> p
  | A n -> p / n
  | S (p',n)  -> (p // p') / n
  | F {f;x} -> F { f = p//f; x }


let (%) f x = F {f;x}

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
    | F {f;x} ->
      let t, f = subst f in
      let t', x = subst x in
      t || t' , F {f;x}
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

let rec prefix = function
  | S(p,_) -> prefix p
  | A n -> n
  | F {f;_} -> prefix f
  | T -> ""

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
