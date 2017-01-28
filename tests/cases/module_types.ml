module type M
= sig
  module A
    : sig
      type t
    end
end
module type MM
= sig
  include M
  val a : A.t
  val b: B.t
end
