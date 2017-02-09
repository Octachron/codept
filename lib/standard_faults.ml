open Fault

(** Warnings *)
let extension_ignored =
  { path = ["extension"; "ignored"];
    expl = "The payload of an extension node was ignored";
  log = (fun lvl ->
      log lvl "%a, extension node @{<m>%s@} ignored." loc)
}

let extension_traversed =
  { path = ["extension"; "traversed"];
    expl = "The payload of the extension node was handled has a standard \
           OCaml code.";
  log = (fun lvl  ->
      log lvl "%a, @ extension node @{<m>%s@} traversed." loc)
}



let opened_first_class =
  { path = ["first_class"; "open"];
    expl= "A first-class module was opened while its signature was deemed \
           unresolved. Consequently, inferred dependendencies after this \
           point may be an over-approximation.";
    log = (fun lvl ->
      log lvl "%a,@ first-class module @{<m>%s@} was opened while its signature was \
               unknown." loc
      )
  }

let included_first_class =
  { path = ["first_class"; "included"];
    expl = "A first-class module was included while its signature was deemed \
           unresolved. Consequently, inferred dependendencies after this \
           point may be an over-approximation.";
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


let applied_unknown =
  { path = ["typing"; "apply"; "unknown"];
    log = (fun lvl l -> log lvl "%a, @ only functor can be applied, hopefully the \
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
    expl = "Signature fault: a module resolution was ambiguous, leading
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
    log = (fun lvl path l ->
        log lvl "%a,@ a non-resolvable module, @{<m>%a@}, has been \
                 replaced by an approximation"
          loc l Paths.S.pp path);
    expl = "Signature fault: an unknown module was approximated, possibly yielding \
            to an over-approximation of dependencies";
  }


let module_conflict =
  { path = ["input"; "module_conflict" ];
    expl = "A module name is provided by multiple sources, only the first one will \
            be used in the following analysis.";
    log = (fun lvl name paths -> log lvl
              "Module conflict,@; Module @{<m>%s@} is provided simultaneously by
@[<hov> %a@]" name Pp.(list ~pre:(s "(") ~sep:(s", ") ~post:(s")") Paths.P.pp) paths
          )
  }


let local_module_conflict =
  { path = ["input"; "module_conflict"; "local" ];
    expl = "A module is provided by multiple input files, only the first one will \
            be used in the following analysis.";
    log = (fun lvl name paths -> log lvl
              "Module conflict,@; Module @{<m>%s@} is provided simultaneously by
@[<hov> %a@]" name Pp.(list ~pre:(s "(") ~sep:(s", ") ~post:(s")") Paths.P.pp) paths
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
        Pp.(list string) lower
        Pp.(list string) diff
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

let m2l_syntaxerr =
  { path = ["parsing"; "m2l"];
    expl = "Parsing fault: syntax error when parsing a m2l serialized file.";
    log = (fun lvl -> log lvl
              "Parsing fault: syntax error when parsing the m2l serialized \
               file @{<loc>%s@}, this should not happen."
          )
  }
