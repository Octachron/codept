(** Test what happens in presence of imperfectly resolved modules *)

module M = struct
  module A = struct end
  include Ext
end

module X = M.F(M.A)

module type f = sig module F:functor (X:sig end) -> sig end end

let f () =
  let module M = struct module F(X:sig end) = struct end end in
  (module M: f)

module N = struct
  module A = struct end
  include (val f ())
end

module Y = N.F(N.A)

module Exact= struct end

module Z = Exact()
