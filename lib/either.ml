  type ('a,'b) t = Left of 'a | Right of 'b
  let map left right = function
    | Left l -> left l
    | Right r -> right r
