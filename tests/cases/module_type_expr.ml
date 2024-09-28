module type T = sig
  module type s = module type of One.Make(Two)
  module M : s
  open M
  open Three
end
