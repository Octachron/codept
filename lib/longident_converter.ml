module L = Longident

let from_lid x =
  let open Paths.Expr in
  let proj = function
    | [] -> None
    | l -> Some l in
  let rec pathlike acc = function
    | L.Lident s -> pure (s::acc)
    | L.Ldot (lid,s) -> pathlike (s::acc) lid
    | L.Lapply (f,x) ->
      app (pathlike [] f) (pathlike [] x) (proj acc) in
  pathlike [] x

let me_from_lid x =
  let open M2l in
  let rec pathlike acc l : module_expr =
    match l with
    | L.Lident s -> Ident (List.rev (s::acc))
    | L.Ldot (lid,s) -> pathlike (s::acc) lid
    | L.Lapply (f,x) ->
      let app =  Apply {f=pathlike [] f; x=pathlike [] x} in
      match acc with
        | [] -> app
        | _ :: _ as proj -> Proj {me=app;proj}
  in
  pathlike [] x
