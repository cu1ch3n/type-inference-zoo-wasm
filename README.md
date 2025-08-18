# Type Inference Zoo

A comprehensive collection of type inference algorithms implemented in Haskell, designed for educational and research purposes. This project provides implementations of various type inference algorithms with detailed derivation trees and LaTeX output.

## Overview

Type Inference Zoo is a research and educational tool that implements multiple type inference algorithms for the lambda calculus with various type system extensions. Each algorithm produces detailed derivation trees showing the step-by-step type inference process, making it ideal for understanding how different type inference strategies work.

## Features

- **Multiple Algorithms**: Implements 8 different type inference algorithms
- **Detailed Derivations**: Each algorithm produces step-by-step derivation trees
- **LaTeX Output**: All expressions and types are formatted in LaTeX for beautiful rendering
- **WASM Integration**: Can be compiled to WebAssembly for web-based usage
- **JSON Protocol**: Standardized JSON output format for easy integration
- **Educational Focus**: Designed to help understand type inference concepts

## Supported Algorithms

### 1. Algorithm W (Hindley-Milner)
- **Module**: `Alg.HM.AlgW`
- **Description**: The classic Hindley-Milner type inference algorithm
- **Features**: 
  - Polymorphic type inference
  - Let-polymorphism
  - Principal types
- **Use Case**: Standard ML-style type inference

### 2. Algorithm R (Fully Grounding)
- **Module**: `Alg.HM.AlgR`
- **Description**: Fully grounding type inference algorithm
- **Features**:
  - Grounds all type variables to concrete types
  - Useful for debugging and understanding type inference
- **Use Case**: Educational purposes and debugging

### 3. DK (Complete and Easy Bidirectional)
- **Module**: `Alg.DK.DK`
- **Description**: Complete and Easy bidirectional type checking
- **Features**:
  - Bidirectional type checking
  - Synthesis and checking modes
  - Complete type inference
- **Use Case**: Modern functional language type systems

### 4. Worklist DK (Higher-Ranked Polymorphic)
- **Module**: `Alg.DK.Worklist.DK`
- **Description**: Higher-ranked polymorphic type inference using worklist algorithm
- **Features**:
  - Higher-ranked polymorphism
  - Worklist-based constraint solving
  - Subtyping constraints
- **Use Case**: Advanced type systems with higher-ranked types

### 5. Elementary Type Inference
- **Module**: `Alg.DK.Worklist.Elementary`
- **Description**: Elementary type inference algorithm
- **Features**:
  - Elementary type system
  - Constraint-based approach
  - Subtyping with bounded quantification
- **Use Case**: Research and educational purposes

### 6. Bounded (Greedy Implicit Bounded Quantification)
- **Module**: `Alg.DK.Worklist.Bounded`
- **Description**: Greedy implicit bounded quantification
- **Features**:
  - Bounded quantification
  - Greedy constraint solving
  - Implicit bounds inference
- **Use Case**: Advanced type systems with bounded polymorphism

### 7. IU (Bidirectional with Intersection and Union Types)
- **Module**: `Alg.DK.Worklist.IU`
- **Description**: Bidirectional type checking with intersection and union types
- **Features**:
  - Intersection types
  - Union types
  - Bidirectional checking
- **Use Case**: Type systems with intersection and union types

### 8. Contextual Typing
- **Module**: `Alg.Local.Contextual.Contextual`
- **Description**: Contextual typing algorithm
- **Features**:
  - Context-aware type inference
  - Local type inference
  - Context matching
- **Use Case**: Local type inference scenarios

## Type System

The project supports a rich type system including:

- **Base Types**: `Int`, `Bool`
- **Type Variables**: `a`, `b`, `c`, etc.
- **Function Types**: `a → b`
- **Universal Types**: `∀a.τ`
- **Intersection Types**: `τ₁ ∩ τ₂`
- **Union Types**: `τ₁ ∪ τ₂`
- **Tuple Types**: `(τ₁, τ₂, ...)`
- **Top/Bottom**: `⊤`, `⊥`

## JSON Protocol

The project uses a standardized JSON protocol for input/output communication. All algorithms produce results in the following format:

### Input Format
```bash
infer --alg <algorithm> <expression>
```

### Output Format
```json
{
  "success": boolean,
  "finalType": "string (optional, the inferred type)",
  "derivation": [
    {
      "ruleId": "string (typing rule name)",
      "expression": "string (LaTeX expression)",
      "children": "Derivation[] (optional, for tree view)"
    }
  ],
  "error": "string (optional, error message)",
  "errorLatex": boolean
}
```

### Example Outputs

#### Successful Inference (Algorithm W)
```json
{
  "success": true,
  "finalType": "a \\to a",
  "derivation": [
    {
      "ruleId": "Abs",
      "expression": "\\vdash \\lambda x.~x : a \\to a",
      "children": [
        {
          "ruleId": "Var",
          "expression": "x: a \\vdash x : a"
        }
      ]
    }
  ]
}
```

#### Linear Derivation (Worklist DK)
```json
{
  "success": true,
  "finalType": "\\text{Int}",
  "derivation": [
    {
      "ruleId": "InfAnn",
      "expression": "\\cdot \\vdash (\\lambda x.~x)~1 : \\text{Int} \\Rightarrow_a \\text{Out}(a)"
    },
    {
      "ruleId": "ChkSub",
      "expression": "\\cdot \\vdash \\text{Out}(\\text{Int}) \\vdash (\\lambda x.~x)~1 \\Leftarrow \\text{Int}"
    }
  ]
}
```

#### Error Case
```json
{
  "success": false,
  "error": "Cannot unify a with a \\to b (occurs check)",
  "derivation": []
}
```

## Installation and Usage

### Prerequisites
- GHC (Glasgow Haskell Compiler) 8.10 or later
- Stack or Cabal

### Building with Stack
```bash
git clone https://github.com/cu1ch3n/type-inference-zoo.git
cd type-inference-zoo
stack build
```

### Building with Cabal
```bash
git clone https://github.com/cu1ch3n/type-inference-zoo.git
cd type-inference-zoo
cabal build
```

### Running
```bash
# Using stack
stack exec type-inference-zoo-exe -- --alg W "(\x. x) 1"

# Using cabal
cabal run type-inference-zoo-exe -- --alg W "(\x. x) 1"
```

### WebAssembly Build
To build for WebAssembly (for web integration):

```bash
# Build WASM version
ghc -O2 -threaded --make Main.hs -o bin.wasm
```

## WASM Integration

The project supports WebAssembly integration for web-based usage. See `WASM_PROTOCOL.md` for detailed integration instructions.

### WASM Features
- **Cross-platform**: Runs in any modern web browser
- **Fast execution**: Near-native performance
- **Easy integration**: Simple JSON protocol
- **Status monitoring**: Connection status indicators

## Project Structure

```
src/
├── Alg/                    # Algorithm implementations
│   ├── HM/                # Hindley-Milner algorithms
│   │   ├── AlgW.hs        # Algorithm W
│   │   └── AlgR.hs        # Algorithm R
│   ├── DK/                # DK algorithms
│   │   ├── DK.hs          # Basic DK
│   │   └── Worklist/      # Worklist-based algorithms
│   │       ├── DK.hs      # Worklist DK
│   │       ├── Elementary.hs
│   │       ├── Bounded.hs
│   │       └── IU.hs
│   └── Local/             # Local type inference
│       └── Contextual/    # Contextual typing
├── Lib.hs                 # Core library functions
├── Syntax.hs              # Type and term definitions
├── Parser.hs              # Expression parser
└── Opt.hs                 # Command-line options
```

## Dependencies

- **aeson**: JSON encoding/decoding
- **megaparsec**: Parser combinator library
- **unbound-generics**: Name binding and substitution
- **containers**: Data structures
- **mtl**: Monad transformers
- **tree-view**: Tree visualization

## Contributing

Contributions are welcome! Please feel free to submit pull requests or open issues for:

- New type inference algorithms
- Bug fixes
- Documentation improvements
- Performance optimizations

## License

This project is licensed under the MIT License - see the `LICENSE` file for details.

## Author

- **Chen Cui** - *Initial work* - [cu1ch3n](https://github.com/cu1ch3n)

## Acknowledgments

This project is inspired by various research papers and implementations in type theory and programming language research. Special thanks to the programming languages research community for their foundational work on type inference algorithms.

## References

- Hindley, J. R. (1969). "The principal type-scheme of an object in combinatory logic"
- Milner, R. (1978). "A theory of type polymorphism in programming"
- Dunfield, J., & Krishnaswami, N. R. (2013). "Complete and easy bidirectional typechecking for higher-rank polymorphism"
- Various papers on higher-ranked polymorphism, intersection types, and contextual typing