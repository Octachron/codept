
module A = struct module M1 = struct end end
module B = struct module M2 = struct end end
module C = struct module M3 = struct end end
module D = struct module M4 = struct end end
module E = struct module M5 = struct end end
module F = struct module M6 = struct end end
module G = struct module M7 = struct end end

module type s1 = module type of A
module type s2 = module type of B
module type s3 = module type of C
module type s4 = module type of D
module type s5 = module type of E
module type s6 = module type of F
module type s7 = module type of G


type a = (module s1)
type b = (module s2)
type c = (module s3)
type d = (module s4)
type e = (module s5)
type f = (module s6)
type g = (module s7)

type ca = a * [`S of cb | `X2 of cb | `X3 of cc | `X5 of ce ]
and cb =  b * [`S of cc | `X2 of cd | `X3 of cf | `X5 of cc ]
and cc =  c * [`S of cd | `X2 of cf | `X3 of cb | `X5 of ca ]
and cd =  d * [`S of ce | `X2 of ca | `X3 of ce | `X5 of cf ]
and ce =  e * [`S of cf | `X2 of cc | `X3 of ca | `X5 of cd ]
and cf =  f * [`S of cg | `X2 of ce | `X3 of cd | `X5 of cb ]
and cg =  g * [`S of ca | `X2 of cg | `X3 of cg | `X5 of cg ]


type _ core =
  | A: ca core
  | B: cb core
  | C: cc core
  | D: cd core
  | E: ce core
  | F: cf core
  | G: cg core


let extract (type a s x2 x3 x5):  (a * [ `S of s | `X2 of x2 | `X3 of x3 | `X5 of x5 ] ) core -> a =
  function
  | A -> (module A : s1)
  | B -> (module B : s2)
  | C -> (module C : s3)
  | D -> (module D : s4)
  | E -> (module E : s5)
  | F -> (module F : s6)
  | G -> (module G : s7)


let succ (type a s x2 x3 x5):
  (a * [ `S of s | `X2 of x2 | `X3 of x3 | `X5 of x5 ] ) core -> s core =
  function
  | A -> B
  | B -> C
  | C -> D
  | E -> F
  | D -> E
  | F -> G
  | G -> A

let x2 (type a s x2 x3 x5):
  (a * [ `S of s | `X2 of x2 | `X3 of x3 | `X5 of x5 ] ) core -> x2 core =
  function
  | A -> B
  | B -> D
  | C -> F
  | D -> A
  | E -> C
  | F -> E
  | G -> G

let x3 (type a s x2 x3 x5):
  (a * [ `S of s | `X2 of x2 | `X3 of x3 | `X5 of x5 ] ) core -> x3 core =
  function
  | A -> C
  | B -> F
  | C -> B
  | D -> E
  | E -> A
  | F -> D
  | G -> G

let x5 (type a s x2 x3 x5):
  (a * [ `S of s | `X2 of x2 | `X3 of x3 | `X5 of x5 ] ) core -> x5 core =
  function
  | A -> E
  | B -> C
  | C -> A
  | D -> F
  | E -> D
  | F -> B
  | G -> G


let s1 x = succ x
let s2 x = s1 @@ succ x
let s3 x = s2 @@ s1 x
let s4 x = s3 @@ s1 x
let s5 x = s1 @@ s4 x
let s6 x = s5 @@ s1 x

let x =  x5 @@ s2 @@ x3 @@ s5 @@ x2 @@ s1 @@ A

let () =
  let (module M) = extract x in
  let open M in
  let open M5 in
  ()
