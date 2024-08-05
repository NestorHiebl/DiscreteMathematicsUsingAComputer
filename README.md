# Discrete Mathematics Using a Computer
## Modernized supplementary material and exercises

Discrete Mathematics Using a Computer is a 2006 book by Cordelia Hall, John
O'Donnel and Rex Page. It is intended as a self-study companion, and comes
with a number of practical exercises as well as supplementary software, which is
available for download
[here](https://www.dcs.gla.ac.uk/~jtod/discrete-mathematics/).

The software in question comes in the form of a literate Haskell module named
`stdm.lhs`, and was originally intended for use with Hugs 98 - a Haskell
compiler and REPL which has since fallen out of use and is no longer in
development. `ghci` is listed as a viable alternative, but the code in the
module does not compile on the current-day `ghc` implementation, even when
Haskell 98 is explicitly named as the language standard version.

Thankfully, the modifications required to modernize the code are relatively
minor. The `stdm.lhs` file in this repository contains all the necessary changes
and should compile out of the box.

The reason this repository exists is to lower the book's barrier to entry. If
you're interested in working through it and are having trouble with the bundled
software, hopefully you found this, thus saving yourself from having to debug
decades old code.

One more note - the function 
`equivalenceRelation :: (Eq a, Show a) => Digraph a -> Digraph a` named in
section 10.9 of the book is missing from the bundled software module. This repo
provides an implementation.

The exercises found here are mostly only those whose solution has the form of
Haskell code. Others, such as proofs by induction or natural deduction may be
added one day.

