(lang dune 2.5)
(using menhir 2.0)

(name codept)
(version 0.12.0)
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
  (depends dune (codept-lib (= :version)) (codept-bundled (= :version)))
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
  (name codept-bundled)
  (license OCaml-LGPL-linking-exception)
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
  (license OCaml-LGPL-linking-exception)
  (synopsis "Alternative ocaml dependency analyzer")
  (depends dune (menhir (>= 20180523)))
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
