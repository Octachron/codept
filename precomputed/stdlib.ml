let signature= let open Module in  let open Sig in 
of_list [M {name="Arg"; origin=Unit {source=Special "stdlib"; file=["Arg"]}; args=[]; signature=empty}; 
        M {name="Array"; origin=Unit {source=Special "stdlib"; file=["Array"]}; args=[]; signature=empty}; 
        M {name="ArrayLabels"; origin=Unit {source=Special "stdlib"; file=["ArrayLabels"]}; args=[]; signature=empty}; 
        M {name="Buffer"; origin=Unit {source=Special "stdlib"; file=["Buffer"]}; args=[]; signature=empty}; 
        M {name="Bytes"; origin=Unit {source=Special "stdlib"; file=["Bytes"]}; args=[]; signature=empty}; 
        M {name="BytesLabels"; origin=Unit {source=Special "stdlib"; file=["BytesLabels"]}; args=[]; signature=empty}; 
        M {name="Callback"; origin=Unit {source=Special "stdlib"; file=["Callback"]}; args=[]; signature=empty}; 
        M {name="CamlinternalFormat"; origin=Unit {source=Special "stdlib"; file=["CamlinternalFormat"]}; args=[]; signature=empty}; 
        M {name="CamlinternalFormatBasics"; origin=Unit {source=Special "stdlib"; file=["CamlinternalFormatBasics"]}; args=[]; signature=empty}; 
        M {name="CamlinternalLazy"; origin=Unit {source=Special "stdlib"; file=["CamlinternalLazy"]}; args=[]; signature=empty}; 
        M {name="CamlinternalMod"; origin=Unit {source=Special "stdlib"; file=["CamlinternalMod"]}; args=[]; signature=empty}; 
        M {name="CamlinternalOO"; origin=Unit {source=Special "stdlib"; file=["CamlinternalOO"]}; args=[]; signature=empty}; 
        M {name="Char"; origin=Unit {source=Special "stdlib"; file=["Char"]}; args=[]; signature=empty}; 
        M {name="Complex"; origin=Unit {source=Special "stdlib"; file=["Complex"]}; args=[]; signature=empty}; 
        M {name="Digest"; origin=Unit {source=Special "stdlib"; file=["Digest"]}; args=[]; signature=empty}; 
        M {name="Ephemeron"; origin=Unit {source=Special "stdlib"; file=["Ephemeron"]}; args=[]; signature=
          (merge 
          (of_list [M {name="GenHashTable"; origin=Submodule; args=[]; signature=of_list 
                      [M {name="MakeSeeded"; origin=Submodule; args=[Some 
                         {name="H"; origin=Submodule; args=[]; signature=empty}]; signature=empty}]}; 
          M {name="K1"; origin=Submodule; args=[]; signature=of_list 
            [M {name="Make"; origin=Submodule; args=[Some 
               {name="H"; origin=Submodule; args=[]; signature=empty}]; signature=empty}; 
            M {name="MakeSeeded"; origin=Submodule; args=[Some 
              {name="H"; origin=Submodule; args=[]; signature=empty}]; signature=empty}]}; 
          M {name="K2"; origin=Submodule; args=[]; signature=of_list 
            [M {name="Make"; origin=Submodule; args=[Some 
               {name="H1"; origin=Submodule; args=[]; signature=empty}; 
               Some {name="H2"; origin=Submodule; args=[]; signature=empty}]; signature=empty}; 
            M {name="MakeSeeded"; origin=Submodule; args=[Some 
              {name="H1"; origin=Submodule; args=[]; signature=empty}; 
              Some {name="H2"; origin=Submodule; args=[]; signature=empty}]; signature=empty}]}; 
          M {name="Kn"; origin=Submodule; args=[]; signature=of_list 
            [M {name="Make"; origin=Submodule; args=[Some 
               {name="H"; origin=Submodule; args=[]; signature=empty}]; signature=empty}; 
            M {name="MakeSeeded"; origin=Submodule; args=[Some 
              {name="H"; origin=Submodule; args=[]; signature=empty}]; signature=empty}]}]) 
          (of_list_type [M {name="S"; origin=Submodule; args=[]; signature=empty}; 
          M {name="SeededS"; origin=Submodule; args=[]; signature=empty}])
           )}; 
        M {name="Filename"; origin=Unit {source=Special "stdlib"; file=["Filename"]}; args=[]; signature=empty}; 
        M {name="Format"; origin=Unit {source=Special "stdlib"; file=["Format"]}; args=[]; signature=empty}; 
        M {name="Gc"; origin=Unit {source=Special "stdlib"; file=["Gc"]}; args=[]; signature=empty}; 
        M {name="Genlex"; origin=Unit {source=Special "stdlib"; file=["Genlex"]}; args=[]; signature=empty}; 
        M {name="Hashtbl"; origin=Unit {source=Special "stdlib"; file=["Hashtbl"]}; args=[]; signature=
          (merge 
          (of_list [M {name="Make"; origin=Submodule; args=[Some 
                      {name="H"; origin=Submodule; args=[]; signature=empty}]; signature=empty}; 
          M {name="MakeSeeded"; origin=Submodule; args=[Some 
            {name="H"; origin=Submodule; args=[]; signature=empty}]; signature=empty}]) 
          (of_list_type [M {name="HashedType"; origin=Submodule; args=[]; signature=empty}; 
          M {name="S"; origin=Submodule; args=[]; signature=empty}; 
          M {name="SeededHashedType"; origin=Submodule; args=[]; signature=empty}; 
          M {name="SeededS"; origin=Submodule; args=[]; signature=empty}])
           )}; 
        M {name="Int32"; origin=Unit {source=Special "stdlib"; file=["Int32"]}; args=[]; signature=empty}; 
        M {name="Int64"; origin=Unit {source=Special "stdlib"; file=["Int64"]}; args=[]; signature=empty}; 
        M {name="Lazy"; origin=Unit {source=Special "stdlib"; file=["Lazy"]}; args=[]; signature=empty}; 
        M {name="Lexing"; origin=Unit {source=Special "stdlib"; file=["Lexing"]}; args=[]; signature=empty}; 
        M {name="List"; origin=Unit {source=Special "stdlib"; file=["List"]}; args=[]; signature=empty}; 
        M {name="ListLabels"; origin=Unit {source=Special "stdlib"; file=["ListLabels"]}; args=[]; signature=empty}; 
        M {name="Map"; origin=Unit {source=Special "stdlib"; file=["Map"]}; args=[]; signature=
          (merge 
          (of_list [M {name="Make"; origin=Submodule; args=[Some 
                      {name="Ord"; origin=Submodule; args=[]; signature=empty}]; signature=empty}]) 
          (of_list_type [M {name="OrderedType"; origin=Submodule; args=[]; signature=empty}; 
          M {name="S"; origin=Submodule; args=[]; signature=empty}])
           )}; 
        M {name="Marshal"; origin=Unit {source=Special "stdlib"; file=["Marshal"]}; args=[]; signature=empty}; 
        M {name="MoreLabels"; origin=Unit {source=Special "stdlib"; file=["MoreLabels"]}; args=[]; signature=of_list 
          [M {name="Hashtbl"; origin=Submodule; args=[]; signature=
             (merge 
             (of_list [M {name="Make"; origin=Submodule; args=[Some 
                         {name="H"; origin=Submodule; args=[]; signature=empty}]; signature=empty}; 
             M {name="MakeSeeded"; origin=Submodule; args=[Some 
               {name="H"; origin=Submodule; args=[]; signature=empty}]; signature=empty}]) 
             (of_list_type [M {name="HashedType"; origin=Submodule; args=[]; signature=empty}; 
             M {name="S"; origin=Submodule; args=[]; signature=empty}; 
             M {name="SeededHashedType"; origin=Submodule; args=[]; signature=empty}; 
             M {name="SeededS"; origin=Submodule; args=[]; signature=empty}])
              )}; 
          M {name="Map"; origin=Submodule; args=[]; signature=
            (merge 
            (of_list [M {name="Make"; origin=Submodule; args=[Some 
                        {name="Ord"; origin=Submodule; args=[]; signature=empty}]; signature=empty}]) 
            (of_list_type [M {name="OrderedType"; origin=Submodule; args=[]; signature=empty}; 
            M {name="S"; origin=Submodule; args=[]; signature=empty}])
             )}; 
          M {name="Set"; origin=Submodule; args=[]; signature=
            (merge 
            (of_list [M {name="Make"; origin=Submodule; args=[Some 
                        {name="Ord"; origin=Submodule; args=[]; signature=empty}]; signature=empty}]) 
            (of_list_type [M {name="OrderedType"; origin=Submodule; args=[]; signature=empty}; 
            M {name="S"; origin=Submodule; args=[]; signature=empty}])
             )}]}; 
        M {name="Nativeint"; origin=Unit {source=Special "stdlib"; file=["Nativeint"]}; args=[]; signature=empty}; 
        M {name="Obj"; origin=Unit {source=Special "stdlib"; file=["Obj"]}; args=[]; signature=of_list 
          [M {name="Ephemeron"; origin=Submodule; args=[]; signature=empty}]}; 
        M {name="Oo"; origin=Unit {source=Special "stdlib"; file=["Oo"]}; args=[]; signature=empty}; 
        M {name="Parsing"; origin=Unit {source=Special "stdlib"; file=["Parsing"]}; args=[]; signature=empty}; 
        M {name="Pervasives"; origin=Unit {source=Special "stdlib"; file=["Pervasives"]}; args=[]; signature=of_list 
          [M {name="LargeFile"; origin=Submodule; args=[]; signature=empty}]}; 
        M {name="Printexc"; origin=Unit {source=Special "stdlib"; file=["Printexc"]}; args=[]; signature=of_list 
          [M {name="Slot"; origin=Submodule; args=[]; signature=empty}]}; 
        M {name="Printf"; origin=Unit {source=Special "stdlib"; file=["Printf"]}; args=[]; signature=empty}; 
        M {name="Queue"; origin=Unit {source=Special "stdlib"; file=["Queue"]}; args=[]; signature=empty}; 
        M {name="Random"; origin=Unit {source=Special "stdlib"; file=["Random"]}; args=[]; signature=of_list 
          [M {name="State"; origin=Submodule; args=[]; signature=empty}]}; 
        M {name="Scanf"; origin=Unit {source=Special "stdlib"; file=["Scanf"]}; args=[]; signature=of_list 
          [M {name="Scanning"; origin=Submodule; args=[]; signature=empty}]}; 
        M {name="Set"; origin=Unit {source=Special "stdlib"; file=["Set"]}; args=[]; signature=
          (merge 
          (of_list [M {name="Make"; origin=Submodule; args=[Some 
                      {name="Ord"; origin=Submodule; args=[]; signature=empty}]; signature=empty}]) 
          (of_list_type [M {name="OrderedType"; origin=Submodule; args=[]; signature=empty}; 
          M {name="S"; origin=Submodule; args=[]; signature=empty}])
           )}; 
        M {name="Sort"; origin=Unit {source=Special "stdlib"; file=["Sort"]}; args=[]; signature=empty}; 
        M {name="Spacetime"; origin=Unit {source=Special "stdlib"; file=["Spacetime"]}; args=[]; signature=of_list 
          [M {name="Series"; origin=Submodule; args=[]; signature=empty}; 
          M {name="Snapshot"; origin=Submodule; args=[]; signature=empty}]}; 
        M {name="Stack"; origin=Unit {source=Special "stdlib"; file=["Stack"]}; args=[]; signature=empty}; 
        M {name="StdLabels"; origin=Unit {source=Special "stdlib"; file=["StdLabels"]}; args=[]; signature=of_list 
          [Alias {name="Array";path=["ArrayLabels"];exact=true}; 
          Alias {name="Bytes";path=["BytesLabels"];exact=true}; 
          Alias {name="List";path=["ListLabels"];exact=true}; 
          Alias {name="String";path=["StringLabels"];exact=true}]}; 
        M {name="Stream"; origin=Unit {source=Special "stdlib"; file=["Stream"]}; args=[]; signature=empty}; 
        M {name="String"; origin=Unit {source=Special "stdlib"; file=["String"]}; args=[]; signature=empty}; 
        M {name="StringLabels"; origin=Unit {source=Special "stdlib"; file=["StringLabels"]}; args=[]; signature=empty}; 
        M {name="Sys"; origin=Unit {source=Special "stdlib"; file=["Sys"]}; args=[]; signature=empty}; 
        M {name="Uchar"; origin=Unit {source=Special "stdlib"; file=["Uchar"]}; args=[]; signature=empty}; 
        M {name="Weak"; origin=Unit {source=Special "stdlib"; file=["Weak"]}; args=[]; signature=
          (merge 
          (of_list [M {name="Make"; origin=Submodule; args=[Some 
                      {name="H"; origin=Submodule; args=[]; signature=empty}]; signature=empty}]) 
          (of_list_type [M {name="S"; origin=Submodule; args=[]; signature=empty}])
           )}]
