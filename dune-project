(lang dune 2.8)
(using menhir 2.0)

(name codept)
(version 0.12.1)
(authors "Florian Angeletti <octa@polychoron.fr>")
(maintainers "Florian Angeletti <octa@polychoron.fr>")
(homepage https://github.com/Octachron/codept)
(bug_reports https://github.com/Octachron/codept/issues)
(source (github Octachron/codept))

 (generate_opam_files true)

(package
  (name codept)
  (license GPL-3.0-or-later)
  (synopsis "Alternative ocaml dependency analyzer")
  (depends dune (codept-lib (= :version)))

  (description "Codept intends to be a dependency solver for OCaml project and an alternative to ocamldep. Compared to ocamldep, codept major features are:

 * whole project analysis
 * exhaustive warning and error messages
 * structured format (s-expression or json) for dependencies
 * uniform handling of delayed alias dependencies
 * (experimental) full dependencies,
   when dependencies up to transitive closure are not enough

Both ocamldep and codept computes an over-approximation of the dependencies graph of OCaml project. However, codept uses whole project analysis to reduce the number of fictitious dependencies inferred at the project scale, whereas ocamldep is, by design, limited to local file analysis."
)
)

(package
 (name codept-lib)

 (license "LGPL-2.1-only WITH OCaml-LGPL-linking-exception")

 (synopsis "Alternative ocaml dependency  analyzer")

 (depends dune (menhir (>= 20180523)) (ocaml (and (>= 4.03) (< 5.4~))))

 (description "Codept intends to be a dependency solver for OCaml project and an
  alternative to ocamldep. This package provides the core library used to build
  the codept executable under a more permissive license in order to be easier to
  reuse in other projects")
)
