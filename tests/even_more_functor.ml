module F(X:sig end) = struct module Y = struct end end
module X = struct module Y = struct end end
module Y = E.W(A.F(X.Y))
