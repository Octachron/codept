let modules= let open Module in  let open Sig in 
Dict.of_list [M {name="Condition"; origin=Unit {source={source=Special "stdlib/threads"; file=["Condition"]};path=["Condition"]}; args=[]; signature=empty}; 
               M {name="Event"; origin=Unit {source={source=Special "stdlib/threads"; file=["Event"]};path=["Event"]}; args=[]; signature=empty}; 
               M {name="Marshal"; origin=Unit {source={source=Special "stdlib/threads"; file=["Marshal"]};path=["Marshal"]}; args=[]; signature=empty}; 
               M {name="Mutex"; origin=Unit {source={source=Special "stdlib/threads"; file=["Mutex"]};path=["Mutex"]}; args=[]; signature=empty}; 
               M {name="Pervasives"; origin=Unit {source={source=Special "stdlib/threads"; file=["Pervasives"]};path=["Pervasives"]}; args=[]; signature=of_list 
                 [M {name="LargeFile"; origin=Submodule; args=[]; signature=empty}]}; 
               M {name="Thread"; origin=Unit {source={source=Special "stdlib/threads"; file=["Thread"]};path=["Thread"]}; args=[]; signature=empty}; 
               M {name="ThreadUnix"; origin=Unit {source={source=Special "stdlib/threads"; file=["ThreadUnix"]};path=["ThreadUnix"]}; args=[]; signature=empty}; 
               M {name="Unix"; origin=Unit {source={source=Special "stdlib/threads"; file=["Unix"]};path=["Unix"]}; args=[]; signature=of_list 
                 [M {name="LargeFile"; origin=Submodule; args=[]; signature=empty}]}] 
