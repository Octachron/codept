open Unit

let test = {|
 open I
 module U = struct module I = struct module E = Ext  end end
 open A
 module K = struct open W.S module X = Y end
 let x = B.x
 module F(X:sig end) = struct module N = Xeno end
 open R
 open C
 open U.I
 open E
 open K.X

|}


let std = Format.std_formatter

module File() = struct
  let lex_test = Lexing.from_channel @@ open_in Sys.argv.(1)

  let ast = Parse.implementation lex_test




(*
let () =
  let open Resolver in
  let env = Envt.empty in
  env
  |> open_ (Epath.A "A")
  |> access (Epath.A "B")
  |> open_ (Epath.F {fn = Epath.A "C"; arg = Epath.A "D" } )
  |> print_env
*)

  let () =
    Parse_ml.structure Envt.empty ast
    |> Envt.pp std
end

module Refine() = struct

  let parse st =
    let lex = Lexing.from_string st in
    let ast = Parse.implementation lex in
    Parse_ml.structure Envt.empty ast


  let a = {|
    open B
    open C
    module type S = sig end
|}

let b = {| module B = struct module C = struct end end |}

let env_a = parse a
let env_b = parse b
let env_for_a = Resolver.open_module Envt.empty
    (Module.Sig env_b.Envt.signature)
let deps, u =
  let open Resolver.Refine in
    map D.empty env_for_a env_a.Envt.unresolved.Unresolved.map

let () = Format.printf
    "env A:@;@[<hov2>%a@]\n env B:@;@[%a@]\n env A | env B:%a;%a\n@."
    Envt.pp env_a
    Envt.pp env_b
    Unresolved.pp u
    (Pp.clist Epath.pp) (Resolver.Refine.D.elements deps)

end

module Do = Refine()
