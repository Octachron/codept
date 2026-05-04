module R = struct end
module M = struct let x = 0 end
let () = let module Z = R in ()
let () = let module Z = R in M.x
