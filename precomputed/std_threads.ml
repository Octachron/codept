let signature= let open Module in  let open Sig in 
of_list [M {name="Condition"; origin=Unit {source=Special "stdlib/threads"; file=["Condition"]}; args=[]; signature=empty}; 
        M {name="Event"; origin=Unit {source=Special "stdlib/threads"; file=["Event"]}; args=[]; signature=empty}; 
        M {name="Mutex"; origin=Unit {source=Special "stdlib/threads"; file=["Mutex"]}; args=[]; signature=empty}; 
        M {name="Thread"; origin=Unit {source=Special "stdlib/threads"; file=["Thread"]}; args=[]; signature=empty}]
