Codept intends to be a dependency solver for OCaml project and an alternative to ocamldep.

Both ocamldep and codept computes an over-approximation of the dependencies graph of OCaml project. However, codept uses whole project analysis to reduce the number of fictitious dependencies inferred at the project scale, whereas ocamldep is, by design, limited to local file analysis.

Consequently, bugs notwithstanding, codept can compute exact dependency graph in any situation that does not involve first class modules, and is still reliable in some standard use cases of first class modules.

## Limitations
More precisely, codept start to fail to compute exact dependencies if a first class module whom signature can not be locally inferred at the point of binding is opened or included. Then subsequent access to submodules of this first class module will generate fictitious dependency. For instance, for a file `a.ml` such as
```OCaml
(* a.ml *)
module type S = sig module B: sig end end
let f (x : (module S)) = x
let g x =
  let (module M) = f x in
  let open M in
  let open B in
  ()

```
`codept a.ml` generate a fictitious `B` dependency and a warning
```
[Warning]:
  First-class module M was opened while its signature was unknown.
```

#Codept overview

In more details, `codept` works by combining together three main ingredients:

- an AST, called M2l, specialized to handle only module level constructions
  (see `M2l.mli` )

- an interruptible interpreter which given an environment and a
  `m2l` Ast computes either the signature represented by the m2l ast, or in
  presence of non-resolved dependencies, a simplified m2l ast.
  (see `Interpreter.ml`)

- a family of environment modules that can for instance track dependencies on the
  fly, search for signature of included library toplevel modules, or approximate
  unknowable modules (see `Envts.ml`).

Currently, these three elements are then used in a basic solver (see `solver.ml`). Given a list of ".ml" and ".mli" files and a starting environment, this basic solver iterates over the list of unresolved files and try to compute their signature.

If the computation is successful, the resulting signature is added to the current environment and the current file is removed from the list of unresolved files. Otherwise the solver continues its iteration.

Cycles and non-resolvable dependencies are detected when the solver does not make any progress after one cycle of iterations over unresolved files.

#Usage

Codept can be used as a drop-in replacement for ocamldep, on Linux at least.
More tests are needed on other platforms. Unfortunately, most of OCaml build systems
are built around ocamldep limitation and would not benefit directly from replacing ocamldep by codept. A possible exception is self-cycle detection: invoking ocamldep
on the following "a.ml" file
```
(* a.ml *)
open A
```
yields directly a circular dependency error.


##Compatibility with ocamldep
Most of the ocamldep options are also supported by codept. The only ocamldep options not implemented by codept is `-allow-approx`: codept can only process syntactically valid files (if the main OCaml migrates to a menhir parser, it might be worthwile
to reimplement a similar feature in codept).

However, some of the ocamldep options are slightly reinterpreted:

  * `-one-line` is the default and only mode of codept makefile output and
  thus adding this option does nothing.

  * `-as-map <file>` and `-map <file>` are both reinterpreted to use the
  codept specific `-no-alias-deps` option which provides a better handling of
  delayed alias dependencies.

  * `-open <module>` does not open the module `<module>` when analyzing the
    `<module.ml>` or `<module.mli>`

Another possible difference between codept and ocamldep output is codept built-in detection of dependency cycles. Within codept, cycles triggers a fatal error message andstops the current analysis.


##Codept-only options

Some new options explore codept possibilities and intermediary representations

  * ` -m2l` prints the `m2l` intermediary representation of the source files
    rather than their dependencies

  * `-deps` prints a textual representation of the result of codept analysis.

  * `-dot` export the dependency graph in the graphviz format

  * `-L <dir>` tells codept to use the cmi files in directory `<dir>` to
    resolve unknown module names during the analysis.

  * `-pkg <module_name>` is equivalent to `-L $(ocamlfind query module_name)`

  * `-inner-modules`, `-unknown-modules` and `-extern-modules`
    refine the `-modules` option by splitting the list of dependencies
    in three subsets:
      *  inner modules are the one provided to `codept` directly through the
         command line,
      *  external modules are modules discovered due to either the `-pkg`
         or `-L` options or precomputed package (like the standard library),
      *  unknown modules are the one that could not be resolved.

  * `-no-alias-deps` delays alias dependency up to the use point of the alias.
    For instance, in the code fragment `module M = A open M` the `A`
    dependency is recorded only when the module `M` is opened in `open M`
    not during the definition of the alias.

  * `-closed-world` stops the analysis as soon as a non-resolvable module is
    identified. Contrarily, codept default mode assumes that non-resolvable
    module have for signature `sig end` (this approximation can only
    lead to an over-approximation of dependencies).

For a more exhaustive list of options, see `codept -help`.

# Installation

`opam pin add codept https://github.com/Octachron/codept.git`


#Comparison between ocamldep and codept

To precise the difference between ocamldep and codept, ocamldep introduces fictional dependencies when opening foreign submodules within a given unit. For instance, if we have two files, `a.ml`
```OCaml
(* a.ml *)
open B
open C
open D
```
and b.ml
```OCaml
(* b.ml *)
module C = struct end
module D = struct end
```
`ocamldep -modules a.ml b.ml` produces
```
a.ml: B C D
b.ml:
```
In this output, the dependencies `B` and `C` for the unit A are fictitious.
Contrarily, `codept -modules a.ml b.ml` uses all the information available in both file at once and produces the exact dependency graph in this situation.
```
a.ml: B
b.ml:
```

Notwithstanding bugs, codept should compute exact dependency graphs when no first class modules are opened or included. In the presence of opened or included first class modules, codept will still compute the exact dependency graph if the signature of the module is present in the pattern binding the module name, for instance with

```OCaml
(* a.ml *)
module type S = sig module C:sig end end
```

```OCaml
(* b.ml *)
open A
let f (module M:S) =
  let open M in
  let open C in
  ()
```

`codept -modules a.ml b.ml`  produces
```
a.ml:
b.ml: A
```
as expected.

However, if the inference of the first class module signature is more involved, codept will produce an inexact dependency graph:

```OCaml
(* a.ml *)
module type S = sig module B: sig end end
let f (x : (module S)) = x
let g x =
  let (module M) = f x in
  let open M in
  let open B in
  ()

```
gives with `codept -modules a.ml` a fictitious `B` dependency
```
a.ml: B
```
and emits a warning
```
Warning:
  First-class module M was opened while its signature was unknown.
```
To avoid this situation, a possible fix is to add back a signature annotation:

```OCaml
(* a.ml *)
module type S = sig module B: sig end end
let f (x : (module S)) = x
let g x =
  let (module M: S) = f x in
  let open M in
  let open B in
  ()

```
