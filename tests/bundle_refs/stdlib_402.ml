let modules= let open Module in  let open Sig in 
Dict.of_list [("Arg",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"Arg";namespace=["Stdlib"]}};path={name=Unitname.modulize"Arg";namespace=["Stdlib"]}}; signature=empty}));
               ("Array",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"Array";namespace=["Stdlib"]}};path={name=Unitname.modulize"Array";namespace=["Stdlib"]}}; signature=empty}));
               ("ArrayLabels",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"ArrayLabels";namespace=["Stdlib"]}};path={name=Unitname.modulize"ArrayLabels";namespace=["Stdlib"]}}; signature=empty}));
               ("Buffer",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"Buffer";namespace=["Stdlib"]}};path={name=Unitname.modulize"Buffer";namespace=["Stdlib"]}}; signature=empty}));
               ("Bytes",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"Bytes";namespace=["Stdlib"]}};path={name=Unitname.modulize"Bytes";namespace=["Stdlib"]}}; signature=empty}));
               ("BytesLabels",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"BytesLabels";namespace=["Stdlib"]}};path={name=Unitname.modulize"BytesLabels";namespace=["Stdlib"]}}; signature=empty}));
               ("Callback",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"Callback";namespace=["Stdlib"]}};path={name=Unitname.modulize"Callback";namespace=["Stdlib"]}}; signature=empty}));
               ("CamlinternalFormat",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"CamlinternalFormat";namespace=["Stdlib"]}};path={name=Unitname.modulize"CamlinternalFormat";namespace=["Stdlib"]}}; signature=empty}));
               ("CamlinternalFormatBasics",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"CamlinternalFormatBasics";namespace=["Stdlib"]}};path={name=Unitname.modulize"CamlinternalFormatBasics";namespace=["Stdlib"]}}; signature=empty}));
               ("CamlinternalLazy",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"CamlinternalLazy";namespace=["Stdlib"]}};path={name=Unitname.modulize"CamlinternalLazy";namespace=["Stdlib"]}}; signature=empty}));
               ("CamlinternalMod",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"CamlinternalMod";namespace=["Stdlib"]}};path={name=Unitname.modulize"CamlinternalMod";namespace=["Stdlib"]}}; signature=empty}));
               ("CamlinternalOO",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"CamlinternalOO";namespace=["Stdlib"]}};path={name=Unitname.modulize"CamlinternalOO";namespace=["Stdlib"]}}; signature=empty}));
               ("Char",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"Char";namespace=["Stdlib"]}};path={name=Unitname.modulize"Char";namespace=["Stdlib"]}}; signature=empty}));
               ("Complex",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"Complex";namespace=["Stdlib"]}};path={name=Unitname.modulize"Complex";namespace=["Stdlib"]}}; signature=empty}));
               ("Digest",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"Digest";namespace=["Stdlib"]}};path={name=Unitname.modulize"Digest";namespace=["Stdlib"]}}; signature=empty}));
               ("Filename",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"Filename";namespace=["Stdlib"]}};path={name=Unitname.modulize"Filename";namespace=["Stdlib"]}}; signature=empty}));
               ("Format",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"Format";namespace=["Stdlib"]}};path={name=Unitname.modulize"Format";namespace=["Stdlib"]}}; signature=empty}));
               ("Gc",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"Gc";namespace=["Stdlib"]}};path={name=Unitname.modulize"Gc";namespace=["Stdlib"]}}; signature=empty}));
               ("Genlex",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"Genlex";namespace=["Stdlib"]}};path={name=Unitname.modulize"Genlex";namespace=["Stdlib"]}}; signature=empty}));
               ("Hashtbl",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"Hashtbl";namespace=["Stdlib"]}};path={name=Unitname.modulize"Hashtbl";namespace=["Stdlib"]}}; signature=
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
               ("Int32",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"Int32";namespace=["Stdlib"]}};path={name=Unitname.modulize"Int32";namespace=["Stdlib"]}}; signature=empty}));
               ("Int64",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"Int64";namespace=["Stdlib"]}};path={name=Unitname.modulize"Int64";namespace=["Stdlib"]}}; signature=empty}));
               ("Lazy",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"Lazy";namespace=["Stdlib"]}};path={name=Unitname.modulize"Lazy";namespace=["Stdlib"]}}; signature=empty}));
               ("Lexing",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"Lexing";namespace=["Stdlib"]}};path={name=Unitname.modulize"Lexing";namespace=["Stdlib"]}}; signature=empty}));
               ("List",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"List";namespace=["Stdlib"]}};path={name=Unitname.modulize"List";namespace=["Stdlib"]}}; signature=empty}));
               ("ListLabels",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"ListLabels";namespace=["Stdlib"]}};path={name=Unitname.modulize"ListLabels";namespace=["Stdlib"]}}; signature=empty}));
               ("Map",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"Map";namespace=["Stdlib"]}};path={name=Unitname.modulize"Map";namespace=["Stdlib"]}}; signature=
                           (merge 
                           (of_list [("Make",Fun (Some {name=Some "Ord";signature=Sig (
                           {origin=Submodule; signature=empty})},Sig (
                           {origin=Submodule; signature=empty})))]) 
                           (of_list_type [("OrderedType",Sig ({origin=Submodule; signature=empty}));
                           ("S",Sig ({origin=Submodule; signature=empty}))])
                            )}));
               ("Marshal",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"Marshal";namespace=["Stdlib"]}};path={name=Unitname.modulize"Marshal";namespace=["Stdlib"]}}; signature=empty}));
               ("MoreLabels",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"MoreLabels";namespace=["Stdlib"]}};path={name=Unitname.modulize"MoreLabels";namespace=["Stdlib"]}}; signature=of_list 
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
               ("Nativeint",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"Nativeint";namespace=["Stdlib"]}};path={name=Unitname.modulize"Nativeint";namespace=["Stdlib"]}}; signature=empty}));
               ("Obj",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"Obj";namespace=["Stdlib"]}};path={name=Unitname.modulize"Obj";namespace=["Stdlib"]}}; signature=empty}));
               ("Oo",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"Oo";namespace=["Stdlib"]}};path={name=Unitname.modulize"Oo";namespace=["Stdlib"]}}; signature=empty}));
               ("Parsing",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"Parsing";namespace=["Stdlib"]}};path={name=Unitname.modulize"Parsing";namespace=["Stdlib"]}}; signature=empty}));
               ("Pervasives",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"Pervasives";namespace=["Stdlib"]}};path={name=Unitname.modulize"Pervasives";namespace=["Stdlib"]}}; signature=of_list 
                                  [("LargeFile",Sig ({origin=Submodule; signature=empty}))]}));
               ("Printexc",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"Printexc";namespace=["Stdlib"]}};path={name=Unitname.modulize"Printexc";namespace=["Stdlib"]}}; signature=of_list 
                                [("Slot",Sig ({origin=Submodule; signature=empty}))]}));
               ("Printf",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"Printf";namespace=["Stdlib"]}};path={name=Unitname.modulize"Printf";namespace=["Stdlib"]}}; signature=empty}));
               ("Queue",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"Queue";namespace=["Stdlib"]}};path={name=Unitname.modulize"Queue";namespace=["Stdlib"]}}; signature=empty}));
               ("Random",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"Random";namespace=["Stdlib"]}};path={name=Unitname.modulize"Random";namespace=["Stdlib"]}}; signature=of_list 
                              [("State",Sig ({origin=Submodule; signature=empty}))]}));
               ("Scanf",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"Scanf";namespace=["Stdlib"]}};path={name=Unitname.modulize"Scanf";namespace=["Stdlib"]}}; signature=of_list 
                             [("Scanning",Sig ({origin=Submodule; signature=empty}))]}));
               ("Set",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"Set";namespace=["Stdlib"]}};path={name=Unitname.modulize"Set";namespace=["Stdlib"]}}; signature=
                           (merge 
                           (of_list [("Make",Fun (Some {name=Some "Ord";signature=Sig (
                           {origin=Submodule; signature=empty})},Sig (
                           {origin=Submodule; signature=empty})))]) 
                           (of_list_type [("OrderedType",Sig ({origin=Submodule; signature=empty}));
                           ("S",Sig ({origin=Submodule; signature=empty}))])
                            )}));
               ("Sort",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"Sort";namespace=["Stdlib"]}};path={name=Unitname.modulize"Sort";namespace=["Stdlib"]}}; signature=empty}));
               ("Stack",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"Stack";namespace=["Stdlib"]}};path={name=Unitname.modulize"Stack";namespace=["Stdlib"]}}; signature=empty}));
               ("StdLabels",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"StdLabels";namespace=["Stdlib"]}};path={name=Unitname.modulize"StdLabels";namespace=["Stdlib"]}}; signature=of_list 
                                 [("Array",Alias {path=Namespaced.make ~nms:["Stdlib"] "ArrayLabels";phantom=None});
                                 ("Bytes",Alias {path=Namespaced.make ~nms:["Stdlib"] "BytesLabels";phantom=None});
                                 ("List",Alias {path=Namespaced.make ~nms:["Stdlib"] "ListLabels";phantom=None});
                                 ("String",Alias {path=Namespaced.make "StringLabels";phantom=None})]}));
               ("Stream",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"Stream";namespace=["Stdlib"]}};path={name=Unitname.modulize"Stream";namespace=["Stdlib"]}}; signature=empty}));
               ("String",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"String";namespace=["Stdlib"]}};path={name=Unitname.modulize"String";namespace=["Stdlib"]}}; signature=empty}));
               ("StringLabels",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"StringLabels";namespace=["Stdlib"]}};path={name=Unitname.modulize"StringLabels";namespace=["Stdlib"]}}; signature=empty}));
               ("Sys",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"Sys";namespace=["Stdlib"]}};path={name=Unitname.modulize"Sys";namespace=["Stdlib"]}}; signature=empty}));
               ("Weak",Sig ({origin=Unit {source={source=Special "stdlib"; file={name=Unitname.modulize"Weak";namespace=["Stdlib"]}};path={name=Unitname.modulize"Weak";namespace=["Stdlib"]}}; signature=
                            (merge 
                            (of_list [("Make",Fun (Some {name=Some "H";signature=Sig (
                            {origin=Submodule; signature=empty})},Sig (
                            {origin=Submodule; signature=empty})))]) 
                            (of_list_type [("S",Sig ({origin=Submodule; signature=empty}))])
                             )}))] 
