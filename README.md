Codept intends to be a dependency solver for OCaml project and an alternative to ocamldep.

Both ocamldep and codept computes an over-approximation of the dependencies graph of OCaml project. However, codept can compute exact dependencies graph in most cases and requires only small human interventions in the problematic cases.

More precisely, ocamldep introduces fictional dependencies when opening foreign submodules within a given unit. For instance, if we have two files, `a.ml`
```OCaml
open B
open C
open D
```
and b.ml
```OCaml
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

However, if the inference of the first class module signature is more involved, codept will produce inexact dependency graph:

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

#Codept overview

In more details, `codept` works by combining together two main ingredients:

- an AST, called M2l, specialized to handle only module level constructions
  (see `M2l.mli` )

- a familly of interruptible interpreters that given an environment and a
  `m2l` Ast computes either the signature represented by the m2l ast, or in
  presence of non-resolved dependencies, a simplified m2l ast.
  (see `Interpreter.ml`)


  Currently, these two elements are then used in a basic solver (see `solver.ml`).
Given a list of ".ml" and ".mli" files and a starting environment,
this basic solver iters over the list of unresolved files and try to compute
their signature.

If the computation is successful, the resulting signature is
added to the current environment and file removed to the list of unresolved files.
Otherwise the solver continues its iteration.

Cycles and non-resolvable dependencies are detected when the solver does not
make any progress after one cycle of iterations over unresolved files.
