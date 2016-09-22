let fmap f = function
  | Some x -> Some (f x)
  | None -> None

let default value = function
  | None -> value
  | Some x -> x

let (>>|) x f= fmap f x
let (><) opt def = default def opt
