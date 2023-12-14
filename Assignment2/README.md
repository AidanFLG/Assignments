# Assignment Reflection - Programming Language Design and Implementation

## By: Aidan, Jordan, Tony

## Introduction

#### This repository documents a comprehensive exploration of programming language nuances, focusing on LambdaNat5, the Calculator, and Haskell. Throughout this assignment, intriguing questions emerged, shedding light on parsing, language design, and implementation choices.

## Running files

### Run: cabal run LambdaNat test/test.lc
### Run: lua solutions. lc
### Run: ghc -o solutions solutions.hs   Then Run:  ./solutions

## Key Observations and Questions

### Parsing differences between #:# and (#):# raise questions about grammar rules. The failure of #:# might stem from reserved symbols, while (#):# succeeds possibly due to parentheses altering parsing. Changing the grammar to accommodate #:# requires careful consideration to maintain consistency without introducing ambiguity or conflicts. Evaluation of its impact on code and parsing complexities is crucial.

### One major hurdle we faced revolved around executing Lua files within our project. While building the Lua files, running them via cabal resulted in numerous challenges, necessitating a separate download of Lua for successful execution. Cabal's integration and execution of Lua encountered compatibility and dependency issues, prompting the need to acquire Lua externally. This unexpected obstacle highlighted the complexities of managing dependencies and executing external scripts within a project environment, emphasizing the importance of adeptly handling dependencies when integrating external tools or languages.

## Reflect on the differences between LambdaNat5 and the Calculator.

### LambdaNat5 and the Calculator diverge due to their evaluation strategies, notably in arithmetic implementation. LambdaNat5's inability to use a direct expression like (eval' (Plus e1 e2) = (eval' e1) + (eval' e2)) stems from its likely use of call by name evaluation, while the Calculator might employ call by value. Call by name in LambdaNat5 ensures lazy evaluation, delaying computation until needed, adhering closely to lambda calculus principles. Conversely, call by value, assumed in the Calculator, computes arguments before function invocation, prioritizing immediate computation for efficiency. This reflects the languages' distinct objectives: LambdaNat5 emphasizes functional purity, while the Calculator prioritizes efficient arithmetic computation.


## Reflecting on Differences with Haskell

### CComparing LambdaNat5 to Haskell, the disparity in functionality and expressiveness is evident. Writing code in LambdaNat5 versus Haskell during this assignment showcased the stark contrast. While LambdaNat5 offers a simplified subset of functional programming, Haskell boasts a vast array of advanced features and a rich ecosystem. Implementing a functional language with LambdaNat5 revealed notable gaps: the absence of crucial elements like type inference, higher-order functions, pattern matching, and extensive libraries that hinder its completeness. To enhance LambdaNat5 to a Haskell-like level, integrating key features such as type inference, higher-order functions, pattern matching, and expanding standard libraries is essential. Incorporating monads and type classes would further augment its capabilities, closing the gap between LambdaNat5's current state and a more robust functional language like Haskell, elevating its usefulness for developers.

## Added Feature in LambdaNat5

### An additional feature, an exponentiation program, has been integrated into LambdaNat5. This new capability allows for efficient calculation of exponents within expressions, enriching the language's mathematical functionalities. Alongside the existing let bindings, rigorously tested through unit tests and sample program evaluations, this enhancement ensures proper scoping, assignment, and mathematical precision, significantly expanding the language's versatility and computational capabilities. Due to problems implementing this feature, two versions of the project are in the repository: LambdaNat contains the working project and LambdaNat_B contains the implementation for the Power rule.
