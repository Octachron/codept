module type s = sig
  module Def : sig module type T end
  module M: Def.T
end

module Def = struct
  module type T = sig module Inner: sig end end
end

module F(X: s with module Def := Def) = struct
  open X
  open M
  open Inner
end

