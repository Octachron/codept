module type Typed = sig
  module type T
  module Val:T
end

module F(X: Typed) = X.Val


module X = struct
  module Y = struct end
end

module A = struct
  module type T = sig module Y:sig end end (*module type of X*)
  module Val = X
end

module X_alias = F(A)

open X_alias
open Y
