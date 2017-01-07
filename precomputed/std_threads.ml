let signature= let open Module in  let open Sig in 
of_list [M {name="Condition"; precision=Exact; origin=Unit {source=Special "stdlib/threads"; file=["Condition"]}; args=[]; signature=empty}; 
        M {name="Event"; precision=Exact; origin=Unit {source=Special "stdlib/threads"; file=["Event"]}; args=[]; signature=empty}; 
        M {name="Mutex"; precision=Exact; origin=Unit {source=Special "stdlib/threads"; file=["Mutex"]}; args=[]; signature=empty}; 
        M {name="Thread"; precision=Exact; origin=Unit {source=Special "stdlib/threads"; file=["Thread"]}; args=[]; signature=empty}]
