module L = Longident

let from_lid  =
  let open Paths.Expr in
  let rec at_pos path = function
    | L.Lident s -> 0, {path = s::path; args= [] }
    | L.Ldot (lid,s) ->
      let n, p = at_pos (s::path) lid in
      n + 1, p
    | L.Lapply (f,x) ->
      let n, path = at_pos path f in
      let arg = full x in
      n, { path with args = (n, arg) :: path.args  }
  and full x =
    let _, p = at_pos [] x in
    { p with args = List.rev p.args } in
  full
