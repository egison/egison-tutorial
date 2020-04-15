# The Egison Tutorial

This is a repository for a tutorial program of [Egison](https://github.com/egison/egison).
Egison is a pattern-matching-oriented, purely functional programming langauge.
Using Egison, we can represent intuitive patterns for non-free data types such as multisets, sets, graphs, and mathematical expressions.

For more information, please visit [Egison website](http://www.egison.org).

## How to compile and run

```
% cabal install
% egison-tutorial
Egison Tutorial Version 4.0.0
Welcome to Egison Tutorial!
** Information **
We can use a "Tab" key to complete keywords on the interpreter.
If we type a "Tab" key after a closed parenthesis, the next closed parenthesis will be completed.
*****************
==============================
List of sections in the tutorial.
1: Arithmetic
2: Basics of functional programming
3: Basics of pattern matching
4: Pattern matching for multisets and sets
5: Symbolic computation
6: Differential geometry: tensor analysis
7: Differential geometry: differential forms
==============================
Choose a section to learn.
(1-7): 4
====================
We can describe pattern matching for multisets and sets.
We can change the interpretation of patterns by changing the matcher, the second argument of the matchAll expression).
The meaning of the cons pattern (::) is generalized to divide a collection into "an" element and the rest.

Examples:
  matchAll [1, 2, 3] as list integer with $x :: $xs -> (x, xs)
  matchAll [1, 2, 3] as multiset integer with $x :: $xs -> (x, xs)
  matchAll [1, 2, 3] as set integer with $x :: $xs -> (x, xs)
====================
> matchAll [1, 2, 3] as list integer with $x :: $xs -> (x, xs)
[(1, [2, 3])]
```
