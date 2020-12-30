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
