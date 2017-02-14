Codept intends to be a dependency solver for OCaml project and an alternative to ocamldep. Compared to ocamldep, codept major features are:

* whole project analysis
* exhaustive warning and error messages
* uniform handling of delayed alias dependencies
* full dependencies, when dependencies up to transitive closure are not enough

Both ocamldep and codept computes an over-approximation of the dependencies graph of OCaml project. However, codept uses whole project analysis to reduce the number of fictitious dependencies inferred at the project scale, whereas ocamldep is, by design, limited to local file analysis.

Consequently, bugs notwithstanding, codept computes an exact dependency graph in any situation that does not involve first class modules, and is still reliable in some standard use cases of first class modules (see this [riddle](tests/case/riddle.ml) as an illustration of why first class modules can be problematic).

Moreover, codept will emit warning messages any time it encounters a source of potential inaccuracies in the dependency graph might be introduced: if no warnings are emitted by codept, the computed dependencies are exact.

Another important point is that codept's whole project analysis feature make it possible to handle uniformly the delayed dependency aspect of module aliases introduced by the `-no-alias-deps` option.

A last point, if dependencies up to transitive closure are not precise
enough, the `-expand-deps` option can track more precisely type aliases induced
dependencies, making it easier to track all cmi files required to compile a given
files for instance..

### Limitations
More precisely, codept starts to fail to compute exact dependencies if a first class module whom signature can not be locally inferred at the point of binding is opened or included. Then subsequent access to submodules of this first class module will generate fictitious dependency. For instance, for a file `a.ml` such as
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
[Warning]: a.ml:l6.11−12,
  First-class module M was opened while its signature was unknown.
```

#Codept overview

In more details, `codept` works by combining together three main ingredients:

- an AST, called M2l, specialized to handle only module level constructions
  (see [M2l](lib/m2l.mli) )

- an interruptible outliner which given an environment and a
  `m2l` Ast computes either the signature represented by the m2l ast, or in
  presence of non-resolved dependencies, a simplified m2l ast.
  (see [Outliner](lib/outliner.mli))

- a family of environment modules that can for instance track dependencies on the
  fly, search for signature of included library toplevel modules, or approximate
  unknowable modules (see [Envts](lib/envts.mli)).

Currently, these three elements are then used in one of the basic solvers
(see [Solver](lib/solver.mli)). Given a list of ".ml" and ".mli" files and a starting environment, the default solver iterates over the list of unresolved files and try to compute their signature.

If the computation is successful, the resulting signature is added to the current environment and the current file is removed from the list of unresolved files. Otherwise the solver continues its iteration.

Cycles and non-resolvable dependencies are detected when the solver does not make any progress after one cycle of iterations over unresolved files.

#Usage

Codept can be used as a drop-in replacement for ocamldep, on Linux at least.
More tests are needed on other platforms. Unfortunately, most of OCaml build systems
are built around ocamldep limitation and would not benefit directly from replacing ocamldep by codept.

A possible exception is self-cycle detection: invoking codept
on the following "a.ml" file
```
(* a.ml *)
open A
```
yields directly a circular dependency error:
```
[Fatal error]: Solver failure
  −Circular dependencies:  A −(a.ml:l2.5−6)⟶ A

```


See the [integration](#integration) section for a better overview
on how to use codept with ocaml build tools.


##Compatibility with ocamldep
Most of the ocamldep options are also supported by codept.

However, some of the ocamldep options are slightly reinterpreted:

  * `-as-map <file>` and `-map <file>` are both reinterpreted to use the
  codept specific `-no-alias-deps` option which provides a better handling of
  delayed alias dependencies.

  * `-open <module>` does not open the module `<module>` when analyzing the
    `<module.ml>` or `<module.mli>`

  * `-allow-approx` use a new experimental heuristic for parsing syntactically
    invalid file that might be more precise − or brittle. More tests are needed.
    See also the more generic `-k` option.

Another possible difference between codept and ocamldep output is codept built-in detection of dependency cycles. With default options, cycles triggers a fatal error message within codept and stops the current analysis. If the option `-k` is specified,
the analysis goes on by ignoring the submodule structure of cycle when inside the cycle.


##Codept-only options

Some new options modify the behavior of either the solver or the outliner

  * `-closed-world` stop the analysis as soon as a non-resolvable module is
    identified. Contrarily, codept default mode assumes that non-resolvable
    module have for signature `sig end` (this approximation can only
    lead to an over-approximation of dependencies).


  * `-expand-deps`
    compute exact dependencies, rather than a subset of dependencies  that
    is equivalent to the exact dependency set up to transitive closure

  * `-k`
    ignore most recoverable errors and keep going

  * `-L <dir>` tell codept to use the cmi files in directory `<dir>` to
    resolve unknown module names during the analysis.

  * `-no-alias-deps` delay alias dependency up to the use point of the alias.
    For instance, in the code fragment `module M = A open M` the `A`
    dependency is recorded only when the module `M` is opened in `open M`
    not during the definition of the alias.


   * `-o filename` set the output file for the subsequent modes. Multiple outputs
     can be specified for the same invocation of codept.

   * `-only-ancestors-of modulename` only analyze files which are an ancestor of
     the module `modulename`.Note that the input name is capitalized and extension
     are removed to avoid some discomfort.


   * `-transparent-extension-node bool` decide what to do with extension nodes,
   if `bool` is true, extension node are considered as transparent and analyzed,
   otherwise they are left alone. Note that ocaml built-in extension nodes
   (i.e. `[%extension_constructor … ]` nodes)  are always analyzed and are not
   affected by this option.


  * `-sig-only` deletes the information that are not necessary for computing
  signatures


All options available with `ocamlfind ocamldep` are also available for `codept`, in particular:

  * `-pkg <module_name>` or `package <module_name> ` is equivalent to
   `-L $(ocamlfind query module_name)`

Some new options enables to serialize files in s-expression format for ulterior
uses by codept:

  * `-sig` exports the inferred module signatures in a sexp format that can
  be read directly by codept

  * `-m2l-sexp` exports the m2l ast in sexp format (that can be parsed by codept)


Other new options explore codept possibilities and intermediary representations

  * ` -m2l` prints the `m2l` intermediary representation of the source files
    rather than their dependencies

  * `-info` prints a textual representation of the result of codept analysis.

  * `-dot` export the dependency graph in the graphviz format


  * `-inner-modules`, `-unknown-modules` and `-extern-modules`
    refine the `-modules` option by splitting the list of dependencies
    in three subsets:
      *  inner modules are the one provided to `codept` directly through the
         command line,
      *  external modules are modules discovered due to either the `-pkg`
         or `-L` options or precomputed package (like the standard library),
      *  unknown modules are the one that could not be resolved.


Warning and error messages, referred together as fault messages can be controled
extensively:

  * `-fatal level` set the fatal level for faults.

  * `-fault-doc` lists all possible fault messages

  * `-fault path.name=level` set the level of the fault from
            `info`, `notification`, `warning`, `error` to `critical`.

  * `-verbosity level` selects the minimal level of displayed fault messages.


For a more exhaustive list of options, see the codept's man page.

# Installation

`opam pin add codept https://github.com/Octachron/codept.git`

# Integration with build tools

Like `ocamldep`, codept can be used to generate `.depends` file for integration
with a makefile based infrastructure.

Combining `codept` with `ocamlbuild` currently requires more efforts.
A library is provided by the`codept.ocamlbuild` subpackage to ease
such integration. Depending on the constraints, `codept` can be enabled
by writing the following simple ocamlbuild file
```OCaml
(*myocamlbuild.ml*)
Codept_ocamlbuild.dispatch ()
```
and adding the `-plugin-tag "package(codept.ocamlbuild)"` option to ocamlbuild
command line:
```
ocamlbuild -plugin-tag "package(codept.ocamlbuild)"  …
```

Better integration with existing tools is still a work in progress.


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
and emits a warning (and a notification)
```
[Warning]: a.ml:l6.11−12,
first-class module M was opened while its signature was unknown.
[Notification]: a.ml:l7.11−12,
a non-resolvable module, ⟨B⟩, has been replaced by an approximation
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

# To do

- Improved exterior API for integration in build tools

- Portability

- Bugs tracking

- Proper library documentation

- Improved library packaging
