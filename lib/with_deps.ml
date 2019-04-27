
type 'a t = { deps: Deps.t ; x:'a}
let with_deps deps x = {deps; x}
let ( <+> ) more {deps; x} = {deps = Deps.(more + deps); x }
let no_deps x = with_deps Deps.empty x
let (>>=) x f = x.deps <+> f x.x
let bind = (>>=)

let (>>|) x f = x >>= (fun x -> no_deps @@ f x)
let map = (>>|)
let (<<|) f x  = x >>= (fun x -> no_deps @@ f x)
let (<*>) x y = { deps = Deps.merge x.deps y.deps; x= x.x , y.x}

let deps x = x.deps
let value x = x.x
let unpack {deps; x} = deps, x
let comm {deps; x} = match x with
  | Ok a -> Ok (with_deps deps a)
  | Error b -> Error(with_deps deps b)
