type t =
  | Ident of string
  | Dot of t * string
  | App of t * t

let from_lid x =
  let open Paths.Expr in
  let proj = function
    | [] -> None
    | l -> Some l in
  let rec pathlike acc = function
    | Ident s -> pure (s::acc)
    | Dot (lid,s) -> pathlike (s::acc) lid
    | App (f,x) ->
      app (pathlike [] f) (pathlike [] x) (proj acc) in
  pathlike [] x

let me_from_lid x =
  let rec pathlike acc (l:t) : M2l.module_expr =
    match l with
    | Ident s -> Ident (s::acc)
    | Dot (lid,s) -> pathlike (s::acc) lid
    | App (f,x) ->
      let app =  M2l.Apply {f=pathlike [] f; x=pathlike [] x} in
      match acc with
        | [] -> app
        | _ :: _ as proj -> Proj {me=app;proj}
  in
  pathlike [] x
