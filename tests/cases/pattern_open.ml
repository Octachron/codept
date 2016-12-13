module M = struct module type s = sig module A: sig end end end

let f (M.((module N:s))) =
        let open N in
        let open A in
        ()

let g (E1.((module N:M.s))) =
  let open N in
  let open A in
  let open E4 in
  ()


let w = function
  | E2.(A) -> A
  | E2.(E3.(B)) -> B
