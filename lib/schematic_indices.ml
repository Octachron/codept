open Schematic

module N = struct

  let zero = Var Zn and next = Sn Zn
  let one = Var next and next = Sn next
  let two = Var next and next = Sn next
  let three = Var next and next = Sn next
  let four = Var next and next = Sn next
  let five = Var next and next = Sn next
  let six = Var next and next = Sn next
  let seven = Var next and next = Sn next
  let eight = Var next and next = Sn next
  let nine = Var next and next = Sn next
  let ten = Var next and next = Sn next

end

let one   = let open N in zero
let two   = let open N in zero, one
let three = let open N in zero, one, two
let four  = let open N in zero, one, two, three
let five  = let open N in zero, one, two, three, four
let six   = let open N in zero, one, two, three, four, five
let seven = let open N in zero, one, two, three, four, five, six
let eight = let open N in zero, one, two, three, four, five, six, seven
let nine  = let open N in zero, one, two, three, four, five, six, seven, eight
let ten   = let open N in zero, one, two, three, four, five, six, seven, eight, nine
