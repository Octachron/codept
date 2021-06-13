module type s = sig
  module Alias = A1
end


module E = struct end

module type s' = s with module Alias.Sub = E

