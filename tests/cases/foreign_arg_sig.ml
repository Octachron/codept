module F(X:Ext.s) = struct
  module G(Y:Ext2.s) = Y(X(Y))
end
