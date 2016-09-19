module type S = sig include Ext end
module E = struct end

module M: S with module Inner = E  = struct end

let () =
  let open M.Inner in
  let open M in
  ()
