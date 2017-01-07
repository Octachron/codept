let signature= let open Module in  let open Sig in 
of_list [M {name="Arg"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Arg"]}; args=[]; signature=empty}; 
        M {name="Array"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Array"]}; args=[]; signature=empty}; 
        M {name="ArrayLabels"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["ArrayLabels"]}; args=[]; signature=empty}; 
        M {name="Buffer"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Buffer"]}; args=[]; signature=empty}; 
        M {name="Bytes"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Bytes"]}; args=[]; signature=empty}; 
        M {name="BytesLabels"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["BytesLabels"]}; args=[]; signature=empty}; 
        M {name="Callback"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Callback"]}; args=[]; signature=empty}; 
        M {name="CamlinternalFormat"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["CamlinternalFormat"]}; args=[]; signature=empty}; 
        M {name="CamlinternalFormatBasics"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["CamlinternalFormatBasics"]}; args=[]; signature=empty}; 
        M {name="CamlinternalLazy"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["CamlinternalLazy"]}; args=[]; signature=empty}; 
        M {name="CamlinternalMod"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["CamlinternalMod"]}; args=[]; signature=empty}; 
        M {name="CamlinternalOO"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["CamlinternalOO"]}; args=[]; signature=empty}; 
        M {name="Char"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Char"]}; args=[]; signature=empty}; 
        M {name="Complex"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Complex"]}; args=[]; signature=empty}; 
        M {name="Digest"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Digest"]}; args=[]; signature=empty}; 
        M {name="Ephemeron"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Ephemeron"]}; args=[]; signature=
          (merge 
          (of_list [M {name="GenHashTable"; precision=Exact; origin=Submodule; args=[]; signature=of_list 
                      [M {name="MakeSeeded"; precision=Exact; origin=Submodule; args=[Some 
                         {name="H"; precision=Exact; origin=Submodule; args=[]; signature=empty}]; signature=empty}]}; 
          M {name="K1"; precision=Exact; origin=Submodule; args=[]; signature=of_list 
            [M {name="Make"; precision=Exact; origin=Submodule; args=[Some 
               {name="H"; precision=Exact; origin=Submodule; args=[]; signature=empty}]; signature=empty}; 
            M {name="MakeSeeded"; precision=Exact; origin=Submodule; args=[Some 
              {name="H"; precision=Exact; origin=Submodule; args=[]; signature=empty}]; signature=empty}]}; 
          M {name="K2"; precision=Exact; origin=Submodule; args=[]; signature=of_list 
            [M {name="Make"; precision=Exact; origin=Submodule; args=[Some 
               {name="H1"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
               Some {name="H2"; precision=Exact; origin=Submodule; args=[]; signature=empty}]; signature=empty}; 
            M {name="MakeSeeded"; precision=Exact; origin=Submodule; args=[Some 
              {name="H1"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
              Some {name="H2"; precision=Exact; origin=Submodule; args=[]; signature=empty}]; signature=empty}]}; 
          M {name="Kn"; precision=Exact; origin=Submodule; args=[]; signature=of_list 
            [M {name="Make"; precision=Exact; origin=Submodule; args=[Some 
               {name="H"; precision=Exact; origin=Submodule; args=[]; signature=empty}]; signature=empty}; 
            M {name="MakeSeeded"; precision=Exact; origin=Submodule; args=[Some 
              {name="H"; precision=Exact; origin=Submodule; args=[]; signature=empty}]; signature=empty}]}]) 
          (of_list_type [M {name="S"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
          M {name="SeededS"; precision=Exact; origin=Submodule; args=[]; signature=empty}])
           )}; 
        M {name="Filename"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Filename"]}; args=[]; signature=empty}; 
        M {name="Format"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Format"]}; args=[]; signature=empty}; 
        M {name="Gc"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Gc"]}; args=[]; signature=empty}; 
        M {name="Genlex"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Genlex"]}; args=[]; signature=empty}; 
        M {name="Hashtbl"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Hashtbl"]}; args=[]; signature=
          (merge 
          (of_list [M {name="Make"; precision=Exact; origin=Submodule; args=[Some 
                      {name="H"; precision=Exact; origin=Submodule; args=[]; signature=empty}]; signature=empty}; 
          M {name="MakeSeeded"; precision=Exact; origin=Submodule; args=[Some 
            {name="H"; precision=Exact; origin=Submodule; args=[]; signature=empty}]; signature=empty}]) 
          (of_list_type [M {name="HashedType"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
          M {name="S"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
          M {name="SeededHashedType"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
          M {name="SeededS"; precision=Exact; origin=Submodule; args=[]; signature=empty}])
           )}; 
        M {name="Int32"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Int32"]}; args=[]; signature=empty}; 
        M {name="Int64"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Int64"]}; args=[]; signature=empty}; 
        M {name="Lazy"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Lazy"]}; args=[]; signature=empty}; 
        M {name="Lexing"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Lexing"]}; args=[]; signature=empty}; 
        M {name="List"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["List"]}; args=[]; signature=empty}; 
        M {name="ListLabels"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["ListLabels"]}; args=[]; signature=empty}; 
        M {name="Map"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Map"]}; args=[]; signature=
          (merge 
          (of_list [M {name="Make"; precision=Exact; origin=Submodule; args=[Some 
                      {name="Ord"; precision=Exact; origin=Submodule; args=[]; signature=empty}]; signature=empty}]) 
          (of_list_type [M {name="OrderedType"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
          M {name="S"; precision=Exact; origin=Submodule; args=[]; signature=empty}])
           )}; 
        M {name="Marshal"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Marshal"]}; args=[]; signature=empty}; 
        M {name="MoreLabels"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["MoreLabels"]}; args=[]; signature=of_list 
          [M {name="Hashtbl"; precision=Exact; origin=Submodule; args=[]; signature=
             (merge 
             (of_list [M {name="Make"; precision=Exact; origin=Submodule; args=[Some 
                         {name="H"; precision=Exact; origin=Submodule; args=[]; signature=empty}]; signature=empty}; 
             M {name="MakeSeeded"; precision=Exact; origin=Submodule; args=[Some 
               {name="H"; precision=Exact; origin=Submodule; args=[]; signature=empty}]; signature=empty}]) 
             (of_list_type [M {name="HashedType"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
             M {name="S"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
             M {name="SeededHashedType"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
             M {name="SeededS"; precision=Exact; origin=Submodule; args=[]; signature=empty}])
              )}; 
          M {name="Map"; precision=Exact; origin=Submodule; args=[]; signature=
            (merge 
            (of_list [M {name="Make"; precision=Exact; origin=Submodule; args=[Some 
                        {name="Ord"; precision=Exact; origin=Submodule; args=[]; signature=empty}]; signature=empty}]) 
            (of_list_type [M {name="OrderedType"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
            M {name="S"; precision=Exact; origin=Submodule; args=[]; signature=empty}])
             )}; 
          M {name="Set"; precision=Exact; origin=Submodule; args=[]; signature=
            (merge 
            (of_list [M {name="Make"; precision=Exact; origin=Submodule; args=[Some 
                        {name="Ord"; precision=Exact; origin=Submodule; args=[]; signature=empty}]; signature=empty}]) 
            (of_list_type [M {name="OrderedType"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
            M {name="S"; precision=Exact; origin=Submodule; args=[]; signature=empty}])
             )}]}; 
        M {name="Nativeint"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Nativeint"]}; args=[]; signature=empty}; 
        M {name="Obj"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Obj"]}; args=[]; signature=of_list 
          [M {name="Ephemeron"; precision=Exact; origin=Submodule; args=[]; signature=empty}]}; 
        M {name="Oo"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Oo"]}; args=[]; signature=empty}; 
        M {name="Parsing"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Parsing"]}; args=[]; signature=empty}; 
        M {name="Pervasives"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Pervasives"]}; args=[]; signature=of_list 
          [M {name="LargeFile"; precision=Exact; origin=Submodule; args=[]; signature=empty}]}; 
        M {name="Printexc"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Printexc"]}; args=[]; signature=of_list 
          [M {name="Slot"; precision=Exact; origin=Submodule; args=[]; signature=empty}]}; 
        M {name="Printf"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Printf"]}; args=[]; signature=empty}; 
        M {name="Queue"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Queue"]}; args=[]; signature=empty}; 
        M {name="Random"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Random"]}; args=[]; signature=of_list 
          [M {name="State"; precision=Exact; origin=Submodule; args=[]; signature=empty}]}; 
        M {name="Scanf"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Scanf"]}; args=[]; signature=of_list 
          [M {name="Scanning"; precision=Exact; origin=Submodule; args=[]; signature=empty}]}; 
        M {name="Set"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Set"]}; args=[]; signature=
          (merge 
          (of_list [M {name="Make"; precision=Exact; origin=Submodule; args=[Some 
                      {name="Ord"; precision=Exact; origin=Submodule; args=[]; signature=empty}]; signature=empty}]) 
          (of_list_type [M {name="OrderedType"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
          M {name="S"; precision=Exact; origin=Submodule; args=[]; signature=empty}])
           )}; 
        M {name="Sort"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Sort"]}; args=[]; signature=empty}; 
        M {name="Spacetime"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Spacetime"]}; args=[]; signature=of_list 
          [M {name="Series"; precision=Exact; origin=Submodule; args=[]; signature=empty}; 
          M {name="Snapshot"; precision=Exact; origin=Submodule; args=[]; signature=empty}]}; 
        M {name="Stack"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Stack"]}; args=[]; signature=empty}; 
        M {name="StdLabels"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["StdLabels"]}; args=[]; signature=of_list 
          [Alias {name="Array";path=["ArrayLabels"]}; 
          Alias {name="Bytes";path=["BytesLabels"]}; 
          Alias {name="List";path=["ListLabels"]}; 
          Alias {name="String";path=["StringLabels"]}]}; 
        M {name="Stream"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Stream"]}; args=[]; signature=empty}; 
        M {name="String"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["String"]}; args=[]; signature=empty}; 
        M {name="StringLabels"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["StringLabels"]}; args=[]; signature=empty}; 
        M {name="Sys"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Sys"]}; args=[]; signature=empty}; 
        M {name="Uchar"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Uchar"]}; args=[]; signature=empty}; 
        M {name="Weak"; precision=Exact; origin=Unit {source=Special "stdlib"; file=["Weak"]}; args=[]; signature=
          (merge 
          (of_list [M {name="Make"; precision=Exact; origin=Submodule; args=[Some 
                      {name="H"; precision=Exact; origin=Submodule; args=[]; signature=empty}]; signature=empty}]) 
          (of_list_type [M {name="S"; precision=Exact; origin=Submodule; args=[]; signature=empty}])
           )}]
