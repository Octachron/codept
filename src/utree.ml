
type 'a t =
  | Node of 'a t list
  | Val of 'a
  | Skip of int

exception Mismatch

let rec take_all lift acc l =
  List.fold_left (take_tree lift) acc l
and take_tree lift l = function
  | Node n -> take_all lift l n
  | Val x -> (lift x) :: l
  | Skip _ -> l

type pattern = { ident:Name.t; signature: M2l.module_type option }
type expr = M2l.module_expr
type full = M2l.bind

let first_class: M2l.module_expr = M2l.Opaque []

let id x = x
let lift_pattern {ident; signature } =
  let me =
    let open Option in
    signature >>| (fun sg ->
    M2l.Constraint(first_class,sg ))
    >< first_class in
  { M2l.name = ident; expr=me }

let merge p expr =
  let open M2l in
  match p.signature, expr with
  | Some s, Constraint(me,_) -> { M2l.name = p.ident; expr = Constraint(me,s) }
  | Some s, me -> { M2l.name = p.ident; expr = Constraint(me,s) }
  | None, expr -> { M2l.name = p.ident; expr }

let rec nodes ((bs, (mes:M2l.module_expr list)) as c) pattern expr =
  match pattern, expr with
  | [], [] -> bs, mes
  | [], _ :: _ | _ :: _, [] -> raise Mismatch
  | p  , Skip 0 :: q -> nodes c p q
  | Skip 0 :: q, e -> nodes c q e
  | Skip n :: p, Skip k :: q ->
    if n >= k then
      nodes c (Skip(n-k) :: p) q
    else
      nodes c p ( Skip(k-n) :: q)
  | Skip k :: p, a :: q ->
    nodes (bs, take_tree id mes a) (Skip (k-1) :: p) q
  | a :: p, (Skip k) :: q ->
    nodes (take_tree lift_pattern bs a, mes) p (Skip (k-1) :: q )
  | Node np :: p, Node ne :: e ->
    let c' = nodes c np ne in
    nodes c' p e
  | Val p :: ps, Val e :: es ->
    nodes (merge p e :: bs, mes) ps es
  | Val _ :: _ , Node _ :: _ | Node _ :: _ , Val _ :: _ ->
    raise Mismatch

let nodes = nodes([],[])

let associate p e = match p, e with
  | Node p, Node e -> nodes p e
  | Val p, Val e -> ([merge p e],[])
  | _, _ -> raise Mismatch
