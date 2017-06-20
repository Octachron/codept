let modules = let open Module in  let open Sig in 
Dict.of_list [M {name="Graphics"; origin=Unit {source=Special "stdlib/graphic"; file=["Graphics"]}; args=[]; signature=empty}; 
        M {name="GraphicsX11"; origin=Unit {source=Special "stdlib/graphic"; file=["GraphicsX11"]}; args=[]; signature=empty}]
