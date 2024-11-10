# Free Foil Higher-Order Unification Benchmark Suite

Towards Generic Higher-Order Unification Implementations in Haskell

## About

This project aims to develop a comprehensive benchmark suite for higher-order
unification (HOU) algorithms, along with an extended lambda calculus interpreter
that supports metavariables, metavariable substitutions, and unification
constraints. It is based on [Free Foil](https://github.com/fizruk/free-foil) and
extends its capabilities to facilitate the testing and evaluation of HOU
algorithms.

**Current Features:**

- **Extended Lambda calculus syntax:** Supports metavariables and unification
  constraints.

- **Test suite in TOML format:** Defines unification problems and expected
  solutions in a structured, human-readable format.


## Getting Started

### Prerequisites

- **Haskell Stack:** Ensure you have the Haskell Stack tool installed. You can
  download it from [here](https://docs.haskellstack.org/en/stable/README/).

- **BNFC (Backus-Naur Form Converter):** Required for generating the parser from
  the grammar. Install it using preffered package manager or from [the BNFC
  GitHub repository](https://github.com/BNFC/bnfc).

### Building the Project

If you make changes to the syntax (e.g., modifying `Syntax.cf`), you need to
regenerate the parser:

```bash
bnfc --haskell -d Syntax.cf -p Language.Lambda -o src
```

Then, build the project using Stack:

```bash
stack clean
stack build
```

### Running the Project

Run test suite with:

```bash
stack run
```

## Usage

### Defining Unification Problems

Currently, unification problems are defined in TOML `config.toml` file. Each
problem consists of constraints and expected solutions.

#### Example Problem:

```toml
language = "untyped λ-calculus"
fragment = "pattern"

[[problems]]
constraints = [
    "∀ f, x . X[f, x] = f Y[x]",
    "∀ x . Y[x] = x x"
]

[[problems.solutions]]
name = "most general"
substitutions = [
    "Y[x] ↦ x x",
    "X[f, x] ↦ f (x x)",
]
```

Explanation:

- Constraints: Equations that need to be unified under certain quantifiers.
- Solutions: Expected substitutions that satisfy the constraints.

### Running Unification Tests

Currently, we do not have HOU algorithms integrated with the test suite. The
unification test mechanism at this stage works as follows:

1. The provided solution substitutions are applied to the defined unification
   problem.
2. The left-hand side (LHS) and right-hand side (RHS) of each constraint are
   evaluated.
3. Both sides undergo beta reduction to simplify the expressions.
4. The simplified LHS and RHS are compared to check if they are alpha-equivalent
   (i.e., identical up to renaming of bound variables).

To run the unification tests:

1. Add Your Unification Problems:
   - Place your unification problems in TOML format within the `config.toml`.

2. Execute the Tests:

   ```bash
   stack run
   ```

## Development

### Modifying the Syntax

If you need to extend or modify the language syntax:

1. Edit the `Syntax.cf` File:
   - Modify `Syntax.cf` to reflect your changes to the language grammar.

2. Regenerate the Parser:

   ```bash
   bnfc --haskell -d Syntax.cf -p Language.Lambda -o src
   ```

3. Build the Project:

   ```bash
   stack clean
   stack build
   ```

## Future Work

- Develop and integrate various HOU algorithms with Free Foil.

- Enhance the benchmark suite with real-world problems to test algorithms in
  contexts similar to actual applications.

- Design the framework to be language-agnostic and extensible, enabling testing
  of various HOU algorithms beyond the current implementation. Allow users to
  specify different languages and logical systems.

- Integrate tools to measure execution time, memory usage, and other performance
  indicators.
