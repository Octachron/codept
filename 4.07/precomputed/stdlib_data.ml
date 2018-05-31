let modules= let open Module in  let open Sig in 
Dict.of_list [M {name="Arg"; origin=Unit {source={source=Special "stdlib"; file=["Arg"]};path=["Arg"]}; args=[]; signature=empty};
               M {name="Array"; origin=Unit {source={source=Special "stdlib"; file=["Array"]};path=["Array"]}; args=[]; signature=of_list 
                 [M {name="Floatarray"; origin=Submodule; args=[]; signature=empty}]};
               M {name="ArrayLabels"; origin=Unit {source={source=Special "stdlib"; file=["ArrayLabels"]};path=["ArrayLabels"]}; args=[]; signature=of_list 
                 [M {name="Floatarray"; origin=Submodule; args=[]; signature=empty}]};
               M {name="Bigarray"; origin=Unit {source={source=Special "stdlib"; file=["Bigarray"]};path=["Bigarray"]}; args=[]; signature=of_list 
                 [M {name="Array0"; origin=Submodule; args=[]; signature=empty};
                 M {name="Array1"; origin=Submodule; args=[]; signature=empty};
                 M {name="Array2"; origin=Submodule; args=[]; signature=empty};
                 M {name="Array3"; origin=Submodule; args=[]; signature=empty};
                 M {name="Genarray"; origin=Submodule; args=[]; signature=empty}]};
               M {name="Buffer"; origin=Unit {source={source=Special "stdlib"; file=["Buffer"]};path=["Buffer"]}; args=[]; signature=empty};
               M {name="Bytes"; origin=Unit {source={source=Special "stdlib"; file=["Bytes"]};path=["Bytes"]}; args=[]; signature=empty};
               M {name="BytesLabels"; origin=Unit {source={source=Special "stdlib"; file=["BytesLabels"]};path=["BytesLabels"]}; args=[]; signature=empty};
               M {name="Callback"; origin=Unit {source={source=Special "stdlib"; file=["Callback"]};path=["Callback"]}; args=[]; signature=empty};
               M {name="CamlinternalFormat"; origin=Unit {source={source=Special "stdlib"; file=["CamlinternalFormat"]};path=["CamlinternalFormat"]}; args=[]; signature=empty};
               M {name="CamlinternalFormatBasics"; origin=Unit {source={source=Special "stdlib"; file=["CamlinternalFormatBasics"]};path=["CamlinternalFormatBasics"]}; args=[]; signature=empty};
               M {name="CamlinternalLazy"; origin=Unit {source={source=Special "stdlib"; file=["CamlinternalLazy"]};path=["CamlinternalLazy"]}; args=[]; signature=empty};
               M {name="CamlinternalMod"; origin=Unit {source={source=Special "stdlib"; file=["CamlinternalMod"]};path=["CamlinternalMod"]}; args=[]; signature=empty};
               M {name="CamlinternalOO"; origin=Unit {source={source=Special "stdlib"; file=["CamlinternalOO"]};path=["CamlinternalOO"]}; args=[]; signature=empty};
               M {name="Char"; origin=Unit {source={source=Special "stdlib"; file=["Char"]};path=["Char"]}; args=[]; signature=empty};
               M {name="Complex"; origin=Unit {source={source=Special "stdlib"; file=["Complex"]};path=["Complex"]}; args=[]; signature=empty};
               M {name="Digest"; origin=Unit {source={source=Special "stdlib"; file=["Digest"]};path=["Digest"]}; args=[]; signature=empty};
               M {name="Ephemeron"; origin=Unit {source={source=Special "stdlib"; file=["Ephemeron"]};path=["Ephemeron"]}; args=[]; signature=
                 (merge 
                 (of_list [M {name="GenHashTable"; origin=Submodule; args=[]; signature=of_list 
                             [M {name="MakeSeeded"; origin=Submodule; args=[Some 
                                {name="H"; origin=Submodule; args=[]; signature=empty}]; signature=empty}]};
                 M {name="K1"; origin=Submodule; args=[]; signature=of_list 
                   [M {name="Make"; origin=Submodule; args=[Some {name="H"; origin=Submodule; args=[]; signature=empty}]; signature=empty};
                   M {name="MakeSeeded"; origin=Submodule; args=[Some 
                     {name="H"; origin=Submodule; args=[]; signature=empty}]; signature=empty}]};
                 M {name="K2"; origin=Submodule; args=[]; signature=of_list 
                   [M {name="Make"; origin=Submodule; args=[Some {name="H1"; origin=Submodule; args=[]; signature=empty};
                      Some {name="H2"; origin=Submodule; args=[]; signature=empty}]; signature=empty};
                   M {name="MakeSeeded"; origin=Submodule; args=[Some 
                     {name="H1"; origin=Submodule; args=[]; signature=empty};
                     Some {name="H2"; origin=Submodule; args=[]; signature=empty}]; signature=empty}]};
                 M {name="Kn"; origin=Submodule; args=[]; signature=of_list 
                   [M {name="Make"; origin=Submodule; args=[Some {name="H"; origin=Submodule; args=[]; signature=empty}]; signature=empty};
                   M {name="MakeSeeded"; origin=Submodule; args=[Some 
                     {name="H"; origin=Submodule; args=[]; signature=empty}]; signature=empty}]}]) 
                 (of_list_type [M {name="S"; origin=Submodule; args=[]; signature=empty};
                 M {name="SeededS"; origin=Submodule; args=[]; signature=empty}])
                  )};
               M {name="Filename"; origin=Unit {source={source=Special "stdlib"; file=["Filename"]};path=["Filename"]}; args=[]; signature=empty};
               M {name="Float"; origin=Unit {source={source=Special "stdlib"; file=["Float"]};path=["Float"]}; args=[]; signature=of_list 
                 [M {name="Array"; origin=Submodule; args=[]; signature=empty}]};
               M {name="Format"; origin=Unit {source={source=Special "stdlib"; file=["Format"]};path=["Format"]}; args=[]; signature=empty};
               M {name="Gc"; origin=Unit {source={source=Special "stdlib"; file=["Gc"]};path=["Gc"]}; args=[]; signature=empty};
               M {name="Genlex"; origin=Unit {source={source=Special "stdlib"; file=["Genlex"]};path=["Genlex"]}; args=[]; signature=empty};
               M {name="Hashtbl"; origin=Unit {source={source=Special "stdlib"; file=["Hashtbl"]};path=["Hashtbl"]}; args=[]; signature=
                 (merge 
                 (of_list [M {name="Make"; origin=Submodule; args=[Some 
                             {name="H"; origin=Submodule; args=[]; signature=empty}]; signature=empty};
                 M {name="MakeSeeded"; origin=Submodule; args=[Some {name="H"; origin=Submodule; args=[]; signature=empty}]; signature=empty}]) 
                 (of_list_type [M {name="HashedType"; origin=Submodule; args=[]; signature=empty};
                 M {name="S"; origin=Submodule; args=[]; signature=empty};
                 M {name="SeededHashedType"; origin=Submodule; args=[]; signature=empty};
                 M {name="SeededS"; origin=Submodule; args=[]; signature=empty}])
                  )};
               M {name="Int32"; origin=Unit {source={source=Special "stdlib"; file=["Int32"]};path=["Int32"]}; args=[]; signature=empty};
               M {name="Int64"; origin=Unit {source={source=Special "stdlib"; file=["Int64"]};path=["Int64"]}; args=[]; signature=empty};
               M {name="Lazy"; origin=Unit {source={source=Special "stdlib"; file=["Lazy"]};path=["Lazy"]}; args=[]; signature=empty};
               M {name="Lexing"; origin=Unit {source={source=Special "stdlib"; file=["Lexing"]};path=["Lexing"]}; args=[]; signature=empty};
               M {name="List"; origin=Unit {source={source=Special "stdlib"; file=["List"]};path=["List"]}; args=[]; signature=empty};
               M {name="ListLabels"; origin=Unit {source={source=Special "stdlib"; file=["ListLabels"]};path=["ListLabels"]}; args=[]; signature=empty};
               M {name="Map"; origin=Unit {source={source=Special "stdlib"; file=["Map"]};path=["Map"]}; args=[]; signature=
                 (merge 
                 (of_list [M {name="Make"; origin=Submodule; args=[Some 
                             {name="Ord"; origin=Submodule; args=[]; signature=empty}]; signature=empty}]) 
                 (of_list_type [M {name="OrderedType"; origin=Submodule; args=[]; signature=empty};
                 M {name="S"; origin=Submodule; args=[]; signature=empty}]) )};
               M {name="Marshal"; origin=Unit {source={source=Special "stdlib"; file=["Marshal"]};path=["Marshal"]}; args=[]; signature=empty};
               M {name="MoreLabels"; origin=Unit {source={source=Special "stdlib"; file=["MoreLabels"]};path=["MoreLabels"]}; args=[]; signature=of_list 
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
               M {name="Nativeint"; origin=Unit {source={source=Special "stdlib"; file=["Nativeint"]};path=["Nativeint"]}; args=[]; signature=empty};
               M {name="Obj"; origin=Unit {source={source=Special "stdlib"; file=["Obj"]};path=["Obj"]}; args=[]; signature=of_list 
                 [M {name="Ephemeron"; origin=Submodule; args=[]; signature=empty}]};
               M {name="Oo"; origin=Unit {source={source=Special "stdlib"; file=["Oo"]};path=["Oo"]}; args=[]; signature=empty};
               M {name="Parsing"; origin=Unit {source={source=Special "stdlib"; file=["Parsing"]};path=["Parsing"]}; args=[]; signature=empty};
               M {name="Printexc"; origin=Unit {source={source=Special "stdlib"; file=["Printexc"]};path=["Printexc"]}; args=[]; signature=of_list 
                 [M {name="Slot"; origin=Submodule; args=[]; signature=empty}]};
               M {name="Printf"; origin=Unit {source={source=Special "stdlib"; file=["Printf"]};path=["Printf"]}; args=[]; signature=empty};
               M {name="Queue"; origin=Unit {source={source=Special "stdlib"; file=["Queue"]};path=["Queue"]}; args=[]; signature=empty};
               M {name="Random"; origin=Unit {source={source=Special "stdlib"; file=["Random"]};path=["Random"]}; args=[]; signature=of_list 
                 [M {name="State"; origin=Submodule; args=[]; signature=empty}]};
               M {name="Scanf"; origin=Unit {source={source=Special "stdlib"; file=["Scanf"]};path=["Scanf"]}; args=[]; signature=of_list 
                 [M {name="Scanning"; origin=Submodule; args=[]; signature=empty}]};
               M {name="Seq"; origin=Unit {source={source=Special "stdlib"; file=["Seq"]};path=["Seq"]}; args=[]; signature=empty};
               M {name="Set"; origin=Unit {source={source=Special "stdlib"; file=["Set"]};path=["Set"]}; args=[]; signature=
                 (merge 
                 (of_list [M {name="Make"; origin=Submodule; args=[Some 
                             {name="Ord"; origin=Submodule; args=[]; signature=empty}]; signature=empty}]) 
                 (of_list_type [M {name="OrderedType"; origin=Submodule; args=[]; signature=empty};
                 M {name="S"; origin=Submodule; args=[]; signature=empty}]) )};
               M {name="Sort"; origin=Unit {source={source=Special "stdlib"; file=["Sort"]};path=["Sort"]}; args=[]; signature=empty};
               M {name="Spacetime"; origin=Unit {source={source=Special "stdlib"; file=["Spacetime"]};path=["Spacetime"]}; args=[]; signature=of_list 
                 [M {name="Series"; origin=Submodule; args=[]; signature=empty};
                 M {name="Snapshot"; origin=Submodule; args=[]; signature=empty}]};
               M {name="Stack"; origin=Unit {source={source=Special "stdlib"; file=["Stack"]};path=["Stack"]}; args=[]; signature=empty};
               M {name="StdLabels"; origin=Unit {source={source=Special "stdlib"; file=["StdLabels"]};path=["StdLabels"]}; args=[]; signature=of_list 
                 [M {name="Array"; origin=Submodule; args=[]; signature=of_list 
                    [M {name="Floatarray"; origin=Submodule; args=[]; signature=empty}]};
                 Alias {name="Bytes";path=Namespaced.make "BytesLabels";phantom=None;weak=false};
                 Alias {name="List";path=Namespaced.make "ListLabels";phantom=None;weak=false};
                 Alias {name="String";path=Namespaced.make "StringLabels";phantom=None;weak=false}]};
               M {name="Stream"; origin=Unit {source={source=Special "stdlib"; file=["Stream"]};path=["Stream"]}; args=[]; signature=empty};
               M {name="String"; origin=Unit {source={source=Special "stdlib"; file=["String"]};path=["String"]}; args=[]; signature=empty};
               M {name="StringLabels"; origin=Unit {source={source=Special "stdlib"; file=["StringLabels"]};path=["StringLabels"]}; args=[]; signature=empty};
               M {name="Sys"; origin=Unit {source={source=Special "stdlib"; file=["Sys"]};path=["Sys"]}; args=[]; signature=empty};
               M {name="Uchar"; origin=Unit {source={source=Special "stdlib"; file=["Uchar"]};path=["Uchar"]}; args=[]; signature=empty};
               M {name="Weak"; origin=Unit {source={source=Special "stdlib"; file=["Weak"]};path=["Weak"]}; args=[]; signature=
                 (merge 
                 (of_list [M {name="Make"; origin=Submodule; args=[Some 
                             {name="H"; origin=Submodule; args=[]; signature=empty}]; signature=empty}]) 
                 (of_list_type [M {name="S"; origin=Submodule; args=[]; signature=empty}])
                  )}] 
