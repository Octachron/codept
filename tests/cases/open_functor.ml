module X = struct end

module type T = sig
  open One.Make(Two).Sub
  open Three
  module F(X:sig end): sig module Sub: sig module A: sig end end end
  open F(X).Sub
  open A
end
