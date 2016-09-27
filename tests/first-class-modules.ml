module type s = sig module A:sig end end
module M = struct module A = struct end end

let x = (module M: s);;


let () =
  let (module N: s) = x in
  let open N in
  let open A in
  ()


open Mark

let () =
  let (module N) = x in
  let open N in
  let open A in
  ()

include (val x)
