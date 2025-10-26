# 0.12.2

* Support 5.4

# 0.12.1

   * Support 5.2
   * Support 5.3

## packaging changes

   * Expose LGPL library
   * maintainance intents

## Library changes

   * Make the error formatter a parameter
   * Better inequality explanation for modules

## Bug fixes

   * fix `-export` bugs
   * handle open `F(X).Y`

## Internal

    * Improve test stability on Windows

# 0.12.0

## Updates

   * Support 5.0.0 standard library in bundle
   * Support 5.1

## Bug fixes

   * Restore "-sort"

## Internal

  * cleaner library interfaces

# 0.11.1 (22 June 2021)

## Features

   * Support for OCaml 4.14
   
## Bug fixes

   * Synchronize the bundled stdlib with the real one

# 0.11.0 (22 June 2021)

## Features

  * Support for abstract module types:
	Codept is now aware that `Y` is `X.M.Y` and
	not an external module in

```ocaml
  module F(X:sig module type t module M:t end) = X
  module X = struct
    module M = struct
      module Y = struct end
    end
    module type t = module type of M
  end
  open F(X)
  open M
  open Y
```

  * Support for OCaml 4.09, 4.10, 4.11, 4.12, 4.13

  * Support for split compilation units: the interface and
    implementation file does not need to share a path to
    be identified as part of the same compilation unit

## Bug fix

 * Nested with constraints `with A.B. ...` triggers aliases dependency
 in -no-alias-deps mode

```ocaml
module type s = sig module Alias = Deps end
module E = struct end
module type s2 = s with module Alias.Sub = E
```

## Internal

* Switch to the zipper engine
* Fully typed recursive definition for schema
* Extended testsuite
* More precise term subast (no more encoding)

# Version 0.10.3

## Features

* Flatter json and sexp output
* Less noisy ambiguous module warning

## Bug fixes

  * Missing dependencies while matching first-class modules

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
