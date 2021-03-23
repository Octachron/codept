open Fault
let fp = Format.fprintf

let em = Format_tags.(tagged Em)

let locc ppf fmt = fp ppf ("%a"^^fmt) locc
let e1 fmt p ppf x = fp ppf fmt p x
let le fmt ppf l = locc ppf fmt l
let l2 fmt printer ppf (x,y) = locc ppf fmt x printer y
let e2 fmt px py ppf (x,y) = fp ppf fmt px x py y
let l3 fmt py pz ppf (x,y,z) = locc ppf fmt x py y pz z
let e3 fmt px py pz ppf (x,y,z) = fp ppf fmt px x py y pz z
let l4 fmt py pz pw ppf (x,y,z,w) = locc ppf fmt x py y pz z pw w

(** Warnings *)
let extension_ignored =
  info ["extension"; "ignored"]
    "The payload of an extension node was ignored"
    (l2 "extension node %a ignored." em)

let extension_traversed =
  info
    ["extension"; "traversed"]
    "The payload of the extension node was handled as a standard \
     OCaml code."
    (l2 "extension node %a traversed." em)

let opened_first_class =
  info
    ["first_class"; "open"]
    "A first-class module was opened while its signature was deemed \
     unresolved. Consequently, inferred dependendencies after this \
     point may be a superset of the real dependencies."
  (fun ppf (lc,name) -> match name with
    | None ->
      locc ppf
        "a first-class module was opened while its signature was unknown."
        lc
     | Some name ->
       locc ppf
         "first-class module %a was opened while its signature was unknown." lc
         Format_tags.(tagged M) name
  )

let included_first_class =
  info ["first_class"; "included"]
  "A first-class module was included while its signature was deemed \
   unresolved. Consequently, inferred dependendencies after this \
   point may be a superset of the real dependencies"
  (le "first-class module was included while its signature was unknown.")

let applied_structure =
  info ["typing"; "apply"; "structure"]
    "Signature fault: a module that was inferred as not a functor \
     was applied like a functor."
    (l2 "only functor can be applied, got:%a" Module.Partial.pp)

let pp_kind ppf : [ `Abstract | `Functor ] -> _ = function
  | `Abstract -> Pp.string ppf "a abstract module type"
  | `Functor -> Pp.string ppf "a functor"

let included  =
  info ["typing"; "structure_expected"]
    "Signature fault: a signature was expected in this situation."
    (l3 "only a signature can be included@ %a@ is a %a"  Module.Partial.pp_sty pp_kind)

let nonexisting_submodule =
  let mtype ppf lvl = Pp.string ppf
      (if lvl = Module.Module_type then "module type" else "submodule") in
  info ["typing"; "non_existing"; "submodule"]
    "Signature fault: there was an error when looking for a non-existing \
     submodule of a known module"
    (l4
        "module %a does not contain any %a %a"
        Format_tags.(with_tag M Paths.S.pp) mtype Format_tags.(tagged M)
    )

let applied_unknown =
  info ["typing"; "apply"; "unknown"]
    "Signature fault: an unknown module was applied like a functor."
    (l2 "only functor can be applied, hopefully the \
         unknown module (%a) is a functor"
        Module.Partial.pp
    )


let maybe_module qual ppf = function
  | Some s -> Pp.fp ppf "the@ %t@ module %a" qual Format_tags.(tagged M) s
  | None -> Pp.fp ppf "a@ %t@ module" qual

let pp_divergence l ppf (d:Module.Divergence.t) =
  let f x = Pp.fp ppf "the@ opening@ of@ %a,@ at@ location \
                       %a"
      (maybe_module (fun ppf ->
           (match d.origin with
            | First_class_module -> Pp.fp ppf "first class"
            | External -> Pp.fp ppf "external")
         )
      )
      d.root Format_tags.(with_tag Loc x) in
  if fst d.loc = fst l then
    f Loc.pp (snd d.loc)
  else
    f loc d.loc

let ambiguous =
  info [ "typing"; "ambiguous"]
    "Signature fault: a module resolution was ambiguous, leading \
     to potential spurious dependencies in the future"
    (fun ppf (l,name,div) -> locc ppf
        "name resolution for %a@ was@ ambiguous,@ due@ to@ %a.@ \
         Spurious@ dependencies@ might@ be@ inferred@ due@ to@ this@ ambiguity."
        l Format_tags.(tagged M) name (pp_divergence l) div
    )

let unknown_approximated =
  info ["typing"; "unknown"; "approximation"]
    "Signature fault: an unknown module was approximated, possibly \
     leading to an over-approximation of dependencies"
    (l3
       "a non-resolvable %a, %a, has been replaced by an approximation"
       Module.pp_level Format_tags.(tagged M)
    )


let module_conflict =
  info ["input"; "module_conflict"; "global" ]
    "A module path is provided by multiple sources, \
     only the first one will be used in the following \
     analysis."
    (e2 "Global module conflict,@; Module %a is provided \
         simultaneously by %a"
        Format_tags.(with_tag M Namespaced.pp)
        (Pp.in_text_list Format_tags.(with_tag M Paths.P.pp))
    )



let local_module_conflict =
  info ["input"; "module_conflict"; "local" ]
   "A module is provided by multiple input files, \
    only the first one will be used in the following \
    analysis."
   (e2
       "Local module conflict,@; Module %a is provided \
        simultaneously by %a" Format_tags.(with_tag M Namespaced.pp)
       Pp.(in_text_list Format_tags.(with_tag M Paths.P.pp))
   )

let concordant_approximation =
  info ["parsing"; "approximation"; "concordant"]
    "Parsing fault: The signature and dependency of an unit were obtained \
     using the heuristic approximative parser. However, the lower and upper \
     bounds for dependencies are equal: inferred dependencies \
     might be exact."
    (e1
       "@[<v>Approximate parsing of %a.@,\
        However, lower and upper bound agreed upon dependencies.@]"
       Paths.P.pp
    )

let discordant_approximation =
  info ["parsing"; "approximation"; "discordant"]
    "Parsing fault: The signature and dependencies of an unit were obtained \
     using the heuristic approximative parser. Moreover, the lower and upper \
     bounds for dependencies are distincts. Codept will use \
     the dependencies upper bound as a safe over-approximation but \
     dependency problems might arise."
    (e3
       "@[<v>Approximate parsing of %a.@,\
        Computed dependencies: at least @[{%a}@], maybe: @[{%a}@]@]"
       Paths.P.pp Pp.(list Paths.S.pp) Pp.(list Paths.S.pp)
    )


(** Syntax errors *)

let print_loc ppf loc =
  let (msg_file, msg_line, msg_chars, msg_to) =
  ("File \"", "\", line ", ", characters ", "-") in
  let open Location in
  let (file, line, startchar) = get_pos_info loc.loc_start in
  let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar in
    Format.fprintf ppf "%s%a%s%i" msg_file print_filename file msg_line line;
    if startchar >= 0 then
      Format.fprintf ppf "%s%i%s%i" msg_chars startchar msg_to endchar

let syntaxerr =
  let loc ppf err = print_loc ppf (Syntaxerr.location_of_error err) in
  info ["parsing"; "syntax"]
   "Parsing fault: not syntactically valid input file."
   (e1 "@[<v>Syntax error@, @[%a@]@]" loc)

let lexerr =
  info ["parsing"; "lexer"]
   "Parsing fault: not lexically valid input file."
   (e2 "Lexer error in file %a%a" Format_tags.(tagged Loc) (fun _ _ -> ()))


(** Codept internal file format errors *)

let unknown_file_format =
  info ["parsing"; "internal"; "unknown"; "format"]
  "unknown file format, an internal serialized file was expected"
  (e2
     "unknown file format,@ when parsing the supposedly serialized %a file %a"
      Pp.string Format_tags.(tagged Loc)
  )

let future_version =
  let version ppf (mj,mn,p) = fp ppf "(%d.%d.%d)" mj mn p in
  info ["parsing"; "internal"; "future"; "version"]
  "file format from the future"
  (e3
     "file %a@ format version %a is more recent than codept own version %a."
     Format_tags.(tagged Loc) version version
  )

let wrong_file_kind =
  info ["parsing"; "internal"; "wrong"; "kind"] "file type mismatch"
    (e3 "%a,@ file type %a does not match the expected type %a."
       Format_tags.(tagged Loc) Pp.string Pp.string
    )


let parsing_error =
  info ["parsing"; "internal"; "error"] "parsing error"
    (e2 "file %a,@ failed to parse %a" Format_tags.(tagged Loc) Pp.string)


let schematic_errors policy (filename,kind,e) =
  begin match e with
        | Schematic.Ext.Future_version {expected;got} ->
          Fault.raise policy future_version (
            filename,
            (expected.major,expected.minor,expected.patch),
            (got.major,got.minor,got.patch)
          )
        | Unknown_format -> Fault.raise policy unknown_file_format (kind,filename)
        | Parse_error -> Fault.raise policy parsing_error (kind,filename)
        | Mismatched_kind {expected;got} ->
          Fault.raise policy wrong_file_kind (filename,got,expected)
      end
