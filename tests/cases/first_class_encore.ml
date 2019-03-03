module type m = sig module A: sig end end
module M = struct module A = struct end end
let m = (module M:m)
let () =
  let (module M:m) = Ext.(m) in
  let open M in
  let open A in
  ()
