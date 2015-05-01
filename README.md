# hindley-milner
Final project for CIS 194 at Penn<br>
[Samy Lanka](http://github.com/lankas) and [Max McCarthy](http://github.com/maxmcc)

#### About
This project is a complete implementation of ML-style [Hindley-Milner type
inference](https://en.wikipedia.org/wiki/Hindley–Milner_type_system) for the
*Exp* language, a restricted subset of the [polymorphic lambda calculus]
(https://en.wikipedia.org/wiki/System_F), also known as System F. The `Infer`
module contains an implementation of Damas and Milner’s **Algorithm&nbsp;W**,
which forms the basis of type inference for many modern functional languages,
including Haskell and all dialects of ML.

#### Instructions
To print the inferred types of all demonstration expressions in `ghci`, run the
following commands:

```sh
> :l Demo
> demoAll
```

Otherwise, the `demo` and `typeOf` functions, defined in `Demo.hs` and
`Infer.hs`, respectively, allow you to infer the types of arbitrary expressions.

#### Structure
- `Exp.hs` – Definition of the *Exp* language
- `Types.hs` – Definition of the type system
- `Infer.hs` – Implementation of type inference
- `Demo.hs` – Some expressions to try it on!

#### What We Learned
We learned a substantial amount about type theory in order to complete this
project. Some concepts we encountered that were particularly interesting were
the untyped and simply typed lambda calculi, `let`-polymorphism, monotypes
(type variables and arrow types) and polymorphic types (type schemes). We were
also introduced to typing rules and derivations.

In addition, we have a reasonable understanding of type unification and
constraint-based typing schemes, both of which are fundamental to Algorithm W.
Much of this came about by reading the articles mentioned in the bibliography
below.

Finally, we have a much deeper appreciation for Haskell's extended type
inference (which includes typeclasses) and the quality of its error messages!


#### Bibliography
Damas, Luis, and Robin Milner. “Principal Type-Schemes for Functional Programs.”
*9th Symposium on Principles of programming languages (POPL'82)*, 1982. 207–212.

Heeren, Bastiaan, Jurriaan Hage, and Doaiste Swierstra. “Generalizing
Hindley-Milner Type Inference Algorithms.” Technical Report UU-CS-2002-031,
Utrecht University, 2002.
