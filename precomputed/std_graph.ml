let signature= let open Module in  let open Sig in 
of_list [M {name="Graphics"; precision=Exact; origin=Unit {source=Special "stdlib/graphic"; file=["Graphics"]}; args=[]; signature=empty}; 
        M {name="GraphicsX11"; precision=Exact; origin=Unit {source=Special "stdlib/graphic"; file=["GraphicsX11"]}; args=[]; signature=empty}]
