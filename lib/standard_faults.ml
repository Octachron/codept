open Fault

(** Warnings *)
let extension_ignored =
  { path = ["extension"; "ignored"];
    expl = "The payload of an extension node was ignored";
  log = (fun lvl ->
      log lvl "%a, extension node @{<em>%s@} ignored." loc)
}

let extension_traversed =
  { path = ["extension"; "traversed"];
    expl = "The payload of the extension node was handled as a standard \
           OCaml code.";
  log = (fun lvl  ->
      log lvl "%a, @ extension node @{<em>%s@} traversed." loc)
}



let opened_first_class =
  { path = ["first_class"; "open"];
    expl= "A first-class module was opened while its signature was deemed \
           unresolved. Consequently, inferred dependendencies after this \
           point may be a superset of the real dependencies.";
    log = (fun lvl ->
      log lvl "%a,@ first-class module @{<m>%s@} was opened while its signature was \
               unknown." loc
      )
  }

let included_first_class =
  { path = ["first_class"; "included"];
    expl = "A first-class module was included while its signature was deemed \
           unresolved. Consequently, inferred dependendencies after this \
           point may be a superset of the real dependencies";
    log = (fun lvl ->
        log lvl
          "%a, @ first-class module was included while its signature was unknown."
          loc
      )
  }

let applied_structure =
  { path = ["typing"; "apply"; "structure"];
    expl = "Signature fault: a module that was inferred as not a functor \
            was applied like a functor.";
    log = (fun lvl l -> log lvl "%a, @ only functor can be applied, got:%a"
               loc l Module.Partial.pp)
  }

let structure_expected =
  { path = ["typing"; "structure_expected"];
    log = (fun lvl l -> log lvl "%a, @ a structure, i.e. not a functor was \
                                 expected; got:%a"
              loc l
              Module.Partial.pp);
    expl = "Signature fault: a functor was not expected in this situation."
  }

let nonexisting_submodule =
    {
      path = ["typing"; "non_existing"; "submodule"];
      log = (fun lvl l path level name ->
          log lvl "%a,@ module @{<m>%a@} does not contain any %s @{<m>%s@}"
            loc l Paths.S.pp path
            (if level=Module.Module_type then "module type" else "submodule")
            name
        );
      expl="Signature fault: there was an error when looking for a non-existing \
             submodule of a known module"
    }

let applied_unknown =
  { path = ["typing"; "apply"; "unknown"];
    log = (fun lvl l -> log lvl
              "%a, @ only functor can be applied, hopefully the \
               unknown module (%a) is a functor"
              loc l
              Module.Partial.pp);
    expl = "Signature fault: an unknown module was applied like a functor.";
  }


let pp_divergence l ppf (d:Module.Divergence.t) =
  let f x = Pp.fp ppf "the@ opening@ of@ the@ %s@ module @{<m>%s@},@ at@ location \
                       @{<loc>%a@}"
      (match d.origin with
       | First_class_module -> "first class"
       | External -> "external")
      d.root x in
  if fst d.loc = fst l then
    f Loc.pp (snd d.loc)
  else
    f loc d.loc

let ambiguous =
  { path = [ "typing"; "ambiguous"];
    expl = "Signature fault: a module resolution was ambiguous, leading \
      to potential spurious dependencies in the future";
    log = (fun lvl l name div -> log lvl
              "%a,@;name resolution for @{<m>%s@}@ was@ ambiguous,@ due@ to@ %a.@ \
               Spurious@ dependencies@ might@ be@ inferred@ due@ to@ this@ ambiguity."
              loc l
              name (pp_divergence l) div
          )
  }

let unknown_approximated =
  { path = ["typing"; "unknown"; "approximation"];
    log = (fun lvl mlvl name l ->
        log lvl "%a,@ a non-resolvable module%s, @{<m>%s@}, has been \
                 replaced by an approximation"
          loc l (if mlvl = Module.Module_type then " type" else "") name );
    expl = "Signature fault: an unknown module was approximated, possibly \
            leading to an over-approximation of dependencies";
  }





let module_conflict =
  { path = ["input"; "module_conflict"; "global" ];
    expl = "A module path is provided by multiple sources, \
            only the first one will be used in the following \
            analysis.";
    log = (fun lvl name paths -> log lvl
              "Global module conflict,@; Module @{<m>%a@} is provided \
               simultaneously by %a" Namespaced.pp name
              Pp.(in_text_list @@ with_tag "m" Paths.P.pp) paths
          )
  }




let local_module_conflict =
  { path = ["input"; "module_conflict"; "local" ];
    expl = "A module is provided by multiple input files, \
            only the first one will be used in the following \
            analysis.";
    log = (fun lvl name paths -> log lvl
              "Local module conflict,@; Module @{<m>%a@} is provided \
               simultaneously by %a" Namespaced.pp name
              Pp.(in_text_list @@ with_tag "m" Paths.P.pp) paths
          )
  }


let concordant_approximation =
  { path = ["parsing"; "approximation"; "concordant"];
    expl = "Parsing fault: The signature and dependency of an unit were obtained \
            using the heuristic approximative parser. However, the lower and upper \
            bounds for dependencies are equal: inferred dependencies \
            might be exact.";
    log = (fun lvl path -> log lvl
             "Approximate parsing of %a.\n\
              However, lower and upper bound agreed upon dependencies."
        Paths.P.pp path
           )
  }

let discordant_approximation =
  { path = ["parsing"; "approximation"; "discordant"];
    expl = "Parsing fault: The signature and dependencies of an unit were obtained \
            using the heuristic approximative parser. Moreover, the lower and upper \
            bounds for dependencies are distincts. Codept will use \
            the dependencies upper bound as a safe over-approximation but \
            dependency problems might arise.";
    log = (fun lvl path lower diff -> log lvl
               "Approximate parsing of %a.\n\
                Computed dependencies: at least {%a}, maybe: {%a}"
        Paths.P.pp path
        Pp.(list Paths.S.pp) lower
        Pp.(list Paths.S.pp) diff
           )
  }


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
  { path = ["parsing"; "syntax"];
    expl = "Parsing fault: not syntactically valid input file.";
    log = (fun lvl error ->
        log lvl "Syntax error\n %a" print_loc
          (Syntaxerr.location_of_error error)
      )
  }


let lexerr =
  { path = ["parsing"; "lexer"];
    expl = "Parsing fault: not lexically valid input file.";
    log = (fun lvl file _error -> log lvl "Lexer error in file @{<loc>%s@}" file )
  }


(** Codept internal file format errors *)

let unknown_file_format =
  { path = ["parsing"; "internal"; "unknown"; "format"];
    expl = "unknown file format, an internal serialized file was expected";
    log = (fun lvl kind name -> log lvl
              "unknown file format,@ when parsing the supposedly \
               serialized %s file @{<loc>%s@}" kind name
          )
  }

let future_version =
  { path = ["parsing"; "internal"; "future"; "version"];
    expl = "file format from the future";
    log = (fun lvl name (mj,mn,p) (mj',mn',p') -> log lvl
              "file @{<loc>%s@}@ format version (%d.%d.%d) is more recent \
               than codept own version (%d.%d.%d)." name mj' mn' p' mj mn p
          )
  }

let wrong_file_kind =
  { path = ["parsing"; "internal"; "wrong"; "kind"];
    expl = "file type mismatch";
    log = (fun lvl filename got expected ->  log lvl
              "@{<loc>%s@},@ file type %s does not match the expected type %s."
              filename got expected);
  }


let parsing_error =
  { path = ["parsing"; "internal"; "error"];
    expl = "parsing error";
    log = (fun lvl kind filename ->  log lvl
              "file @{<loc>%s@},@ failed to parse %s" filename kind);
  }


let schematic_errors policy (filename,kind,e) =
  begin match e with
        | Schematic.Ext.Future_version {expected;got} ->
          Fault.handle policy future_version
            filename
            (expected.major,expected.minor,expected.patch)
            (got.major,got.minor,got.patch)
        | Unknown_format ->
          Fault.handle policy unknown_file_format
            kind filename
        | Parse_error ->
          Fault.handle policy parsing_error
            kind filename
        | Mismatched_kind {expected;got} ->
          Fault.handle policy wrong_file_kind
            filename got expected
      end
