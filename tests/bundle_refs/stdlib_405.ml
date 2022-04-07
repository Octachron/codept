let modules= let open Module in  let open Sig in 
Dict.of_list [("Arg",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="Arg";namespace=["Stdlib"]}};path={name="Arg";namespace=["Stdlib"]}}; signature=empty}));
               ("Array",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="Array";namespace=["Stdlib"]}};path={name="Array";namespace=["Stdlib"]}}; signature=empty}));
               ("ArrayLabels",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="ArrayLabels";namespace=["Stdlib"]}};path={name="ArrayLabels";namespace=["Stdlib"]}}; signature=empty}));
               ("Buffer",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="Buffer";namespace=["Stdlib"]}};path={name="Buffer";namespace=["Stdlib"]}}; signature=empty}));
               ("Bytes",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="Bytes";namespace=["Stdlib"]}};path={name="Bytes";namespace=["Stdlib"]}}; signature=empty}));
               ("BytesLabels",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="BytesLabels";namespace=["Stdlib"]}};path={name="BytesLabels";namespace=["Stdlib"]}}; signature=empty}));
               ("Callback",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="Callback";namespace=["Stdlib"]}};path={name="Callback";namespace=["Stdlib"]}}; signature=empty}));
               ("CamlinternalFormat",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="CamlinternalFormat";namespace=["Stdlib"]}};path={name="CamlinternalFormat";namespace=["Stdlib"]}}; signature=empty}));
               ("CamlinternalFormatBasics",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="CamlinternalFormatBasics";namespace=["Stdlib"]}};path={name="CamlinternalFormatBasics";namespace=["Stdlib"]}}; signature=empty}));
               ("CamlinternalLazy",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="CamlinternalLazy";namespace=["Stdlib"]}};path={name="CamlinternalLazy";namespace=["Stdlib"]}}; signature=empty}));
               ("CamlinternalMod",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="CamlinternalMod";namespace=["Stdlib"]}};path={name="CamlinternalMod";namespace=["Stdlib"]}}; signature=empty}));
               ("CamlinternalOO",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="CamlinternalOO";namespace=["Stdlib"]}};path={name="CamlinternalOO";namespace=["Stdlib"]}}; signature=empty}));
               ("Char",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="Char";namespace=["Stdlib"]}};path={name="Char";namespace=["Stdlib"]}}; signature=empty}));
               ("Complex",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="Complex";namespace=["Stdlib"]}};path={name="Complex";namespace=["Stdlib"]}}; signature=empty}));
               ("Digest",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="Digest";namespace=["Stdlib"]}};path={name="Digest";namespace=["Stdlib"]}}; signature=empty}));
               ("Ephemeron",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="Ephemeron";namespace=["Stdlib"]}};path={name="Ephemeron";namespace=["Stdlib"]}}; signature=
                                 (merge 
                                 (of_list [("GenHashTable",Sig ({origin=Submodule; signature=of_list 
                                                                [("MakeSeeded",Fun (Some {name=Some "H";signature=Sig (
                                                                {origin=Submodule; signature=empty})},Sig (
                                                                {origin=Submodule; signature=empty})))]}));
                                 ("K1",Sig ({origin=Submodule; signature=of_list 
                                            [("Make",Fun (Some {name=Some "H";signature=Sig (
                                            {origin=Submodule; signature=empty})},Sig (
                                            {origin=Submodule; signature=empty})));
                                            ("MakeSeeded",Fun (Some {name=Some "H";signature=Sig (
                                            {origin=Submodule; signature=empty})},Sig (
                                            {origin=Submodule; signature=empty})))]}));
                                 ("K2",Sig ({origin=Submodule; signature=of_list 
                                            [("Make",Fun (Some {name=Some "H1";signature=Sig (
                                            {origin=Submodule; signature=empty})},Fun (Some {name=Some "H2";signature=Sig (
                                            {origin=Submodule; signature=empty})},Sig (
                                            {origin=Submodule; signature=empty}))));
                                            ("MakeSeeded",Fun (Some {name=Some "H1";signature=Sig (
                                            {origin=Submodule; signature=empty})},Fun (Some {name=Some "H2";signature=Sig (
                                            {origin=Submodule; signature=empty})},Sig (
                                            {origin=Submodule; signature=empty}))))]}));
                                 ("Kn",Sig ({origin=Submodule; signature=of_list 
                                            [("Make",Fun (Some {name=Some "H";signature=Sig (
                                            {origin=Submodule; signature=empty})},Sig (
                                            {origin=Submodule; signature=empty})));
                                            ("MakeSeeded",Fun (Some {name=Some "H";signature=Sig (
                                            {origin=Submodule; signature=empty})},Sig (
                                            {origin=Submodule; signature=empty})))]}))]) 
                                 (of_list_type [("S",Sig ({origin=Submodule; signature=empty}));
                                 ("SeededS",Sig ({origin=Submodule; signature=empty}))])
                                  )}));
               ("Filename",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="Filename";namespace=["Stdlib"]}};path={name="Filename";namespace=["Stdlib"]}}; signature=empty}));
               ("Format",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="Format";namespace=["Stdlib"]}};path={name="Format";namespace=["Stdlib"]}}; signature=empty}));
               ("Gc",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="Gc";namespace=["Stdlib"]}};path={name="Gc";namespace=["Stdlib"]}}; signature=empty}));
               ("Genlex",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="Genlex";namespace=["Stdlib"]}};path={name="Genlex";namespace=["Stdlib"]}}; signature=empty}));
               ("Hashtbl",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="Hashtbl";namespace=["Stdlib"]}};path={name="Hashtbl";namespace=["Stdlib"]}}; signature=
                               (merge 
                               (of_list [("Make",Fun (Some {name=Some "H";signature=Sig (
                               {origin=Submodule; signature=empty})},Sig (
                               {origin=Submodule; signature=empty})));
                               ("MakeSeeded",Fun (Some {name=Some "H";signature=Sig (
                               {origin=Submodule; signature=empty})},Sig (
                               {origin=Submodule; signature=empty})))]) 
                               (of_list_type [("HashedType",Sig ({origin=Submodule; signature=empty}));
                               ("S",Sig ({origin=Submodule; signature=empty}));
                               ("SeededHashedType",Sig ({origin=Submodule; signature=empty}));
                               ("SeededS",Sig ({origin=Submodule; signature=empty}))])
                                )}));
               ("Int32",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="Int32";namespace=["Stdlib"]}};path={name="Int32";namespace=["Stdlib"]}}; signature=empty}));
               ("Int64",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="Int64";namespace=["Stdlib"]}};path={name="Int64";namespace=["Stdlib"]}}; signature=empty}));
               ("Lazy",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="Lazy";namespace=["Stdlib"]}};path={name="Lazy";namespace=["Stdlib"]}}; signature=empty}));
               ("Lexing",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="Lexing";namespace=["Stdlib"]}};path={name="Lexing";namespace=["Stdlib"]}}; signature=empty}));
               ("List",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="List";namespace=["Stdlib"]}};path={name="List";namespace=["Stdlib"]}}; signature=empty}));
               ("ListLabels",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="ListLabels";namespace=["Stdlib"]}};path={name="ListLabels";namespace=["Stdlib"]}}; signature=empty}));
               ("Map",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="Map";namespace=["Stdlib"]}};path={name="Map";namespace=["Stdlib"]}}; signature=
                           (merge 
                           (of_list [("Make",Fun (Some {name=Some "Ord";signature=Sig (
                           {origin=Submodule; signature=empty})},Sig (
                           {origin=Submodule; signature=empty})))]) 
                           (of_list_type [("OrderedType",Sig ({origin=Submodule; signature=empty}));
                           ("S",Sig ({origin=Submodule; signature=empty}))])
                            )}));
               ("Marshal",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="Marshal";namespace=["Stdlib"]}};path={name="Marshal";namespace=["Stdlib"]}}; signature=empty}));
               ("MoreLabels",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="MoreLabels";namespace=["Stdlib"]}};path={name="MoreLabels";namespace=["Stdlib"]}}; signature=of_list 
                                  [("Hashtbl",Sig ({origin=Submodule; signature=
                                                   (merge 
                                                   (of_list [("Make",Fun (Some {name=Some "H";signature=Sig (
                                                   {origin=Submodule; signature=empty})},Sig (
                                                   {origin=Submodule; signature=empty})));
                                                   ("MakeSeeded",Fun (Some {name=Some "H";signature=Sig (
                                                   {origin=Submodule; signature=empty})},Sig (
                                                   {origin=Submodule; signature=empty})))]) 
                                                   (of_list_type [("HashedType",Sig (
                                                   {origin=Submodule; signature=empty}));
                                                   ("S",Sig ({origin=Submodule; signature=empty}));
                                                   ("SeededHashedType",Sig (
                                                   {origin=Submodule; signature=empty}));
                                                   ("SeededS",Sig ({origin=Submodule; signature=empty}))])
                                                    )}));
                                  ("Map",Sig ({origin=Submodule; signature=
                                              (merge 
                                              (of_list [("Make",Fun (Some {name=Some "Ord";signature=Sig (
                                              {origin=Submodule; signature=empty})},Sig (
                                              {origin=Submodule; signature=empty})))]) 
                                              (of_list_type [("OrderedType",Sig (
                                              {origin=Submodule; signature=empty}));
                                              ("S",Sig ({origin=Submodule; signature=empty}))])
                                               )}));
                                  ("Set",Sig ({origin=Submodule; signature=
                                              (merge 
                                              (of_list [("Make",Fun (Some {name=Some "Ord";signature=Sig (
                                              {origin=Submodule; signature=empty})},Sig (
                                              {origin=Submodule; signature=empty})))]) 
                                              (of_list_type [("OrderedType",Sig (
                                              {origin=Submodule; signature=empty}));
                                              ("S",Sig ({origin=Submodule; signature=empty}))])
                                               )}))]}));
               ("Nativeint",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="Nativeint";namespace=["Stdlib"]}};path={name="Nativeint";namespace=["Stdlib"]}}; signature=empty}));
               ("Obj",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="Obj";namespace=["Stdlib"]}};path={name="Obj";namespace=["Stdlib"]}}; signature=of_list 
                           [("Ephemeron",Sig ({origin=Submodule; signature=empty}))]}));
               ("Oo",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="Oo";namespace=["Stdlib"]}};path={name="Oo";namespace=["Stdlib"]}}; signature=empty}));
               ("Parsing",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="Parsing";namespace=["Stdlib"]}};path={name="Parsing";namespace=["Stdlib"]}}; signature=empty}));
               ("Pervasives",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="Pervasives";namespace=["Stdlib"]}};path={name="Pervasives";namespace=["Stdlib"]}}; signature=of_list 
                                  [("LargeFile",Sig ({origin=Submodule; signature=empty}))]}));
               ("Printexc",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="Printexc";namespace=["Stdlib"]}};path={name="Printexc";namespace=["Stdlib"]}}; signature=of_list 
                                [("Slot",Sig ({origin=Submodule; signature=empty}))]}));
               ("Printf",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="Printf";namespace=["Stdlib"]}};path={name="Printf";namespace=["Stdlib"]}}; signature=empty}));
               ("Queue",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="Queue";namespace=["Stdlib"]}};path={name="Queue";namespace=["Stdlib"]}}; signature=empty}));
               ("Random",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="Random";namespace=["Stdlib"]}};path={name="Random";namespace=["Stdlib"]}}; signature=of_list 
                              [("State",Sig ({origin=Submodule; signature=empty}))]}));
               ("Scanf",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="Scanf";namespace=["Stdlib"]}};path={name="Scanf";namespace=["Stdlib"]}}; signature=of_list 
                             [("Scanning",Sig ({origin=Submodule; signature=empty}))]}));
               ("Set",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="Set";namespace=["Stdlib"]}};path={name="Set";namespace=["Stdlib"]}}; signature=
                           (merge 
                           (of_list [("Make",Fun (Some {name=Some "Ord";signature=Sig (
                           {origin=Submodule; signature=empty})},Sig (
                           {origin=Submodule; signature=empty})))]) 
                           (of_list_type [("OrderedType",Sig ({origin=Submodule; signature=empty}));
                           ("S",Sig ({origin=Submodule; signature=empty}))])
                            )}));
               ("Sort",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="Sort";namespace=["Stdlib"]}};path={name="Sort";namespace=["Stdlib"]}}; signature=empty}));
               ("Spacetime",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="Spacetime";namespace=["Stdlib"]}};path={name="Spacetime";namespace=["Stdlib"]}}; signature=of_list 
                                 [("Series",Sig ({origin=Submodule; signature=empty}));
                                 ("Snapshot",Sig ({origin=Submodule; signature=empty}))]}));
               ("Stack",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="Stack";namespace=["Stdlib"]}};path={name="Stack";namespace=["Stdlib"]}}; signature=empty}));
               ("StdLabels",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="StdLabels";namespace=["Stdlib"]}};path={name="StdLabels";namespace=["Stdlib"]}}; signature=of_list 
                                 [("Array",Alias {path=Namespaced.make ~nms:["Stdlib"] "ArrayLabels";phantom=None});
                                 ("Bytes",Alias {path=Namespaced.make ~nms:["Stdlib"] "BytesLabels";phantom=None});
                                 ("List",Alias {path=Namespaced.make ~nms:["Stdlib"] "ListLabels";phantom=None});
                                 ("String",Alias {path=Namespaced.make "StringLabels";phantom=None})]}));
               ("Stream",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="Stream";namespace=["Stdlib"]}};path={name="Stream";namespace=["Stdlib"]}}; signature=empty}));
               ("String",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="String";namespace=["Stdlib"]}};path={name="String";namespace=["Stdlib"]}}; signature=empty}));
               ("StringLabels",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="StringLabels";namespace=["Stdlib"]}};path={name="StringLabels";namespace=["Stdlib"]}}; signature=empty}));
               ("Sys",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="Sys";namespace=["Stdlib"]}};path={name="Sys";namespace=["Stdlib"]}}; signature=empty}));
               ("Uchar",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="Uchar";namespace=["Stdlib"]}};path={name="Uchar";namespace=["Stdlib"]}}; signature=empty}));
               ("Weak",Sig ({origin=Unit {source={source=Special "stdlib"; file={name="Weak";namespace=["Stdlib"]}};path={name="Weak";namespace=["Stdlib"]}}; signature=
                            (merge 
                            (of_list [("Make",Fun (Some {name=Some "H";signature=Sig (
                            {origin=Submodule; signature=empty})},Sig (
                            {origin=Submodule; signature=empty})))]) 
                            (of_list_type [("S",Sig ({origin=Submodule; signature=empty}))])
                             )}))] 
