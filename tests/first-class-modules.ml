module type s = sig module A:sig end module B:sig end end
module M = struct module A = struct end module B = struct end end

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
  let open B in
  ()

include (val x)
