module F(X:sig module type T module M:T end) = X.M

module X = struct
  module M = struct module Inner = Ext end
  module type T=sig module Inner = Ext end
end

let f (x:F(X).Inner.t) = ()
