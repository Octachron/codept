[@@@if 5.5]
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
