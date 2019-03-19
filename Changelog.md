# Version 0.10.3

## Bug fixes

  * Missing dependencies while matching first-class modules
  * Less noisy ambiguous module warning

## Support

  * OCaml 4.08

# Version 0.10.2

## Features

  * Improved priority order for module resolution:
    local files, libraries, standard library
  * support for ocaml script syntax,
    `#open` and `#require` are not taken in account yet.

## OCaml version support:

  * OCaml 4.07:
    * prefixed standard library

## Bug fixes:

  * dot output: escape graphviz keywords

# Version 0.10.1

## Support:

  * OCaml 4.06

## Bugs:

        * Fix "-ppx" option, which could be ignored

# Version 0.10

## Features:

  * nested module hierarchy support
  * structured format (json and s-expression) for deps.
  * structured format for m2l and sig files
  * file groups in command lines input
  * explicit module name ascription: `codept filename.ml:Module_name`

## Bugs:
  * better alias and ε-deps tracking
  * support for module dependencies in with constraints
  * support for `F(X).t` in type expression

# Version 0.9.1
  * `-shared` option to generate dependencies for shared object only
  * Support for OCaml 4.03
  * Support for OCaml 4.05

# Version 0.9
  * Initial release
