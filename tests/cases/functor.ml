module F(X:sig module M: sig end end) = struct end
module type s = sig module M:sig end end
module G(X:s) = struct open X open M end
