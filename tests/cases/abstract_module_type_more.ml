
module type Common = sig
  module type T
  module X:T
end

module F(X:sig include Common end)(Y: sig include Common end)=struct
  module A = X.X
  module B = Y.X
end

module X = struct
  module X = struct module XI=struct end end
  module type T = module type of X
end

module Y = struct
  module X = struct end
  module type T = module type of X
end

module R = F(X)(Y)

open R.A.XI
open R.B.XI

