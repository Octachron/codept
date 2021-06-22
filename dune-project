(lang dune 2.7)
(using menhir 2.0)

(name codept)
(authors "Florian Angeletti <octa@polychoron.fr>")
(maintainers "Florian Angeletti <octa@polychoron.fr>")
(homepage https://github.com/Octachron/codept)
(bug_reports https://github.com/Octachron/codept/issues)
(source (github Octachron/codept))


(package
  (name codept)
  (version 0.11.0)
  (license gpl-3)
  (synopsis "Alternative ocaml dependency analyzer")
  (depends dune menhir)
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
