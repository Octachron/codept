module A = struct
  module B = X
end
module B = struct end
module M = struct module A = struct end end

module type s = sig module A:sig end end

let f (x:(module s)) = ()

let x = (module M : s)
let () =
  let module M = (val x) in
  let open M in
  let open A in (* either .A or M.A: no dependencies *)
  let open B in (* either .A.B or ?B or M.A.B: B X dependencies *)
  ()
