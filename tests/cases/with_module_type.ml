[@@@if 4.13]

module type s = sig
  module type t
  module M: t
end

module F(X: s with module type t = sig module A: sig end end) = struct
  open X
  open M
  open A
end
