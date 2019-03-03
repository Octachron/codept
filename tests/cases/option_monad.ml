[@@@if 4.08]

let ( let* ) value body =
  match value with
  | Some x -> body x
  | None -> None

let ( and* ) x y = match x, y with
| Some x, Some y -> Some(x,y)
| _ -> None

module type m = sig module Inner:sig end end
let value = Some 3
let m: (module m) = (module struct module Inner = struct end end)

let om = Some really

let more =
  let* Ext.(x) = Ext3.(value)
  and* (module M:m) = Ext4.(om) in
  let open M in
  let open Inner in
  Some(2 * x)
