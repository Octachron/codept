module type S = sig include Ext.S end
module type s = sig module Inner:sig end end
module E = struct end

module M: s with module Inner = E  = struct
  module Inner = struct end
end

let () =
  let open M.Inner in
  ()
