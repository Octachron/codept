module R = struct end
module M = struct let x = 0 end
let () = let module Z = R in ()
let () = let module Z = R in M.x

let () =
  let module X = struct
      module Y = struct end
    end
  in
  let open X in
  let open Y in
  ()


let () =
  let module M = struct
      module type T = sig
        module A: sig
          module B: sig end
        end
      end
    end
  in
  let g (module P:M.T) =
    let open P in
    let open A in
    let open B in
    ()
  in
  ()

let () =
  let module type T = sig
      module C: sig
        module D: sig end
      end
    end
  in
  let g (module M:T) =
    let open M in
    let open C in
    let open D in
    ()
  in
  ()
