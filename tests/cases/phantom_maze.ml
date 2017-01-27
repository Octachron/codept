module A = struct
  module B = struct
    module C = X
  end

  module D = struct
    module E = Y
  end
end

open External (* external deps *)

open A (* A? or External.A ?: no deps *)
open B (* external B or A.B? : deps B *)
open C (* external C? or A.B.C ≡ X ?: deps C AND X *)

open D (* external D? or A.D?: deps D *)
open E (* external E? or A.D.E≡Y?: deps E and Y *)
