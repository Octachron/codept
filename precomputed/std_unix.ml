let signature= let open Module in  let open Sig in 
of_list [M {name="Unix"; precision=Exact; origin=Unit {source=Special "stdlib/unix"; file=["Unix"]}; args=[]; signature=of_list 
           [M {name="LargeFile"; precision=Exact; origin=Submodule; args=[]; signature=empty}]}; 
        M {name="UnixLabels"; precision=Exact; origin=Unit {source=Special "stdlib/unix"; file=["UnixLabels"]}; args=[]; signature=of_list 
          [M {name="LargeFile"; precision=Exact; origin=Submodule; args=[]; signature=empty}]}]
