type t =
  | Ident of string
  | Dot of t * string
  | App of t * t

val from_lid: t -> Paths.Expr.t
val me_from_lid: t -> M2l.module_expr
