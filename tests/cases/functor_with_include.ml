
module type s = sig module M:sig end end
module F(X:s)(Y:s) = struct include Y open M end
