(*
module B = struct end
module D = struct
  module type s = sig module S_inner:sig end end
end
module F = struct end
module H = struct
  module type s = sig module H_inner:sig end end
end
module J = struct end
*)

module X = struct
  open B
  module Y(A:C.s): sig end = struct

    open A.S_inner
    open D
    let () =
      let (module Y: E.s) = (module struct module E_inner = struct end end) in
      let open Y in
      let open E_inner in
        let open F in
      ()
  end
end
