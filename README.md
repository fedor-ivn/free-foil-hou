# Free foil matching and higher-order unification

This repository provides generic second-order matching over
[free foil](https://github.com/fizruk/free-foil) syntax, a typed lambda-calculus
adapter, and TOML problem sets. It also contains experimental higher-order
unification algorithms, including Huet preunification.

## Run the matching example

Install [Stack](https://docs.haskellstack.org/), then run these commands from
the repository root:

```sh
stack build
stack exec -- runghc -package=free-foil-hou examples/Matching.hs
```

[The example](examples/Matching.hs) first declares a metavariable `M` with
two arguments of type `t` and result type `t`, then asks the library to solve:

```haskell
matchUnificationConstraint
  ["M : [t, t] t"]
  "∀ x: t. M[x, x] = x"
```

Here `x` is universally quantified. We want a body for `M` that makes the left
side equal to `x` whenever both arguments are `x`. The matcher finds two bodies:

```text
M[a, b] ↦ a
M[a, b] ↦ b
```

The names `a` and `b` are the formal parameters of `M`. The first solution
returns its first argument, so `M[x, x]` becomes `x`. The second returns its
second argument, giving `x` again. These are distinct substitutions: applied
to `M[x, y]`, they would give `x` and `y`, respectively. Since the target is a
variable, a body headed by an object-language operator cannot match it.
Thus the two projections are the only solutions.

The caller supplies the metavariable declaration and the equation. The
lambda-calculus adapter parses them and passes the terms and type contexts
to the generic `match` function in `Data.SOAS`. The matcher computes both
substitutions. It uses free foil to handle bound names and substitution
without variable capture. The same matching algorithm works with other
object-language signatures through the interface described below.

The example also solves two smaller problems:

- `M[] = x` has no solution. With no formal parameters, the body assigned to
  `M` has no way to refer to the universally quantified variable `x`.
- `x = x` succeeds with the empty substitution, because the two sides already
  agree and there is no metavariable to assign.

All variables have the base type `t`. Matching compares syntax up to
alpha-equivalence; it does not identify object-language terms by beta or eta
conversion.

## Use the generic matcher

[`Data.SOAS`](src/Data/SOAS.hs) provides `TypedSOAS`, `MetaApp`, `MetaSubsts`,
`match`, and `applyMetaSubsts`. `match` takes five arguments:

```haskell
match scope metavarTypes varTypes lhs rhs
```

`scope` contains the variables in scope. `metavarTypes` maps each metavariable
name to its argument types and result type. `varTypes` records the type of each
variable. `lhs` is the pattern, and `rhs` is the target term.

The result is a list of simultaneous metavariable substitutions. An empty list
means failure; a list containing `MetaSubsts []` includes a successful branch
that needs no assignments. Several solutions may exist. Metavariables in
unused arguments may remain unassigned.

For the matching problem studied in the paper, the target contains no
metavariables. The API also permits a target signature extension `ext`, whose
nodes are treated as rigid syntax. Returned bodies may contain those rigid
nodes. With a common signature, `applyMetaSubsts` applies a solution to the
pattern, and `alphaEquiv` from `Control.Monad.Free.Foil` compares the result
with the unchanged target.

To use another object language, provide:

- A second-order signature with `Bitraversable` and `ZipMatchK` instances.
- A binder representation with `SinkableK` and `UnifiablePattern` instances.
- Type annotations with `Eq` and `ZipMatchK` instances, and metavariable names
  with `Ord` and `ZipMatchK` instances.
- A `TypedBinder binder t` instance. Its `addBinderTypes` method extends the
  variable type context with the variables introduced by a binder.

Free foil supports deriving the signature and binder instances. The caller
supplies `addBinderTypes`. The independent signature in
[`Data.SOASSpec`](test/hspec/Data/SOASSpec.hs) demonstrates constants,
operators with both binding and non-binding arguments, and multiple binders.

Inputs must be well-scoped, with complete contexts and correct operator and
binder annotations. The matcher checks compared term types and metavariable
arities and types. It does not implement an object-language type checker.

## Run the tests

```sh
stack test --pedantic --ghc-options -O2
```

Matching tests read [`problems/matching.toml`](problems/matching.toml) and
[`problems/matching-regressions.toml`](problems/matching-regressions.toml).
They compare computed and reference solutions and check every computed result
by substitution and alpha-equivalence, without beta reduction. Problems
without reference solutions must produce no solutions.

`Data.SOASSpec` tests the generic matcher directly. It covers constants,
mixed binding and non-binding arguments, multiple bound variables, type and
arity checks, repeated and nested metavariables, alternative projections,
discarded arguments, and renaming of both binder lists.

The suite also tests the lambda-calculus adapter and Huet preunification.

## Add matching problems

Each matching fixture contains metavariable declarations, one constraint,
and zero or more expected solutions. For example, the first problem in
`problems/matching.toml` is:

```toml
[[problems]]
metavars = ["X : [t -> t, t] t"]
constraints = ["∀ f: t -> t, x: t. X[f, x] = f x"]

[[problems.solutions]]
name = "most general"
metavars = []
substitutions = ["X[b, c] ↦ b c"]
```

Omit `[[problems.solutions]]` for a problem with no solutions. Run `stack test`
after editing either matching fixture file.

The existing executable prints solutions and their comparisons with reference
solutions from `problems/matching.toml`:

```sh
stack run free-foil-hou-exe
```

## Development

`stack.yaml` pins the compiler snapshot and free-foil dependency. Stack builds
the BNFC, Alex, and Happy build tools. The custom `Setup.hs` generates the
lambda parser from `Syntax.cf` during configuration, so grammar changes need
reconfiguration:

```sh
stack build --reconfigure
```

To generate API documentation:

```sh
stack haddock --no-haddock-deps
```
