module type s = sig module A: sig end end
module M = struct module A = struct end end

let () = 
  let (module M), (module N)= (module M:s), (module M:s) in
  let open M in
  let open N in
  let open A in
  ()

let () = 
  let [| (module M); (module N) |] = [| (module M:s); (module M:s) |] in
  let open M in
  let open N in
  let open A in
  ()
