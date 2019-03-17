let modules= let open Module in  let open Sig in 
Dict.of_list [M {name="Graphics"; origin=Unit {source={source=Special "stdlib/graph"; file=["Graphics"]};path=["Graphics"]}; args=[]; signature=empty}; 
               M {name="GraphicsX11"; origin=Unit {source={source=Special "stdlib/graph"; file=["GraphicsX11"]};path=["GraphicsX11"]}; args=[]; signature=empty}] 
