let modules= let open Module in  let open Sig in 
Dict.of_list [M {name="Unix"; origin=Unit {source={source=Special "stdlib/unix"; file=["Unix"]};path=["Unix"]}; args=[]; signature=of_list 
                [M {name="LargeFile"; origin=Submodule; args=[]; signature=empty}]}; 
               M {name="UnixLabels"; origin=Unit {source={source=Special "stdlib/unix"; file=["UnixLabels"]};path=["UnixLabels"]}; args=[]; signature=of_list 
                 [M {name="LargeFile"; origin=Submodule; args=[]; signature=empty}]}] 
