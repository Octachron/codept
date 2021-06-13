module E = struct end

module type s = sig
  module M = Ext
end

module type u = s with type M.t = int



module type s2 = sig
  module M = Ext2
end

module type u2 = s2 with module M.Sub = E

module type s3 = u2


module Nested = struct
  module Inner = Ext3
end

module type s = sig module E: sig end end

module type s2 = s
  with module E = Nested
  with type E.Inner.t = int
