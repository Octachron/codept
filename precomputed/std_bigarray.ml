let modules = let open Module in  let open Sig in 
Dict.of_list [M {name="Bigarray"; origin=Unit {source=Special "stdlib/bigarray"; file=["Bigarray"]}; args=[]; signature=of_list 
           [M {name="Array0"; origin=Submodule; args=[]; signature=empty}; 
           M {name="Array1"; origin=Submodule; args=[]; signature=empty}; 
           M {name="Array2"; origin=Submodule; args=[]; signature=empty}; 
           M {name="Array3"; origin=Submodule; args=[]; signature=empty}; 
           M {name="Genarray"; origin=Submodule; args=[]; signature=empty}]}]
