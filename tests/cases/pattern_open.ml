module M = struct module type s = sig module A: sig end end end

let f (M.((module N:s))) =
        let open N in
        let open A in
        ()

module M' = struct module type s' = sig module A': sig end end end


let g (E1.((module N:M'.s'))) =
  let open N in
  let open A' in (* Note that it may happen that s' =  E1.M'.s' *)
  let open E4 in
  ()


let w = function
  | E2.(A) -> A
  | E2.(E3.(B)) -> B
