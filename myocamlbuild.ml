
open Ocamlbuild_plugin

let () =
  dispatch begin function
  | After_rules ->
     flag ["doc"; "ocaml"; "utf-8" ] (S [ A "-charset"; A"utf-8"]);
  | _ -> ()
  end
