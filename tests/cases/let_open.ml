module M = struct module A = struct end end
let () = 
  let open M in
  let open A in
    M
