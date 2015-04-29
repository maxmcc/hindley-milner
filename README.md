# hindley-milner
Final project for CIS 194 at Penn<br>
[Samy Lanka](http://github.com/lankas) and [Max McCarthy](http://github.com/maxmcc)

#### About
This project is a complete implementation of ML-style [Hindley-Milner type inference](https://en.wikipedia.org/wiki/Hindley–Milner_type_system) for the *Exp* language, a restricted subset of the [polymorphic lambda calculus](https://en.wikipedia.org/wiki/System_F), also known as System F. The `Infer` module contains an implementation of Damas and Milner’s **Algorithm&nbsp;W**, which forms the basis of type inference for many modern functional languages, including Haskell and all dialects of ML. [CIS 194](http://www.cis.upenn.edu/~cis194/) is an undergraduate-level introduction to the Haskell programming language.

#### Structure
- `Exp.hs` – Definition of the *Exp* language
- `Types.hs` – Definition of the type system
- `Infer.hs` – Implementation of type inference
- `Demo.hs` – Some expressions to try it on!

#### Bibliography
Damas, Luis, and Robin Milner. “Principal Type-Schemes for Functional Programs.” *9th Symposium on Principles of programming languages (POPL'82)*, 1982. 207–212.

Heeren, Bastiaan, Jurriaan Hage, and Doaiste Swierstra. “Generalizing Hindley-Milner Type Inference Algorithms.” Technical Report UU-CS-2002-031, Utrecht University, 2002.
