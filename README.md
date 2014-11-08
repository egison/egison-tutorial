# The Egison Totorial

This is a repository for a tutorial program of [Egison](https://github.com/egisatoshi/egison).
Egison is the pattern-matching oriented, purely functional programming langauge.
With Egison, we can represent pattern-matching with unfree data types intuitively, especially for collection data, such as lists, multisets, sets.
Please try Egison with this tutorial program!

For more information, please visit [Egison website](http://www.egison.org).

## How to compile and run

```
% cabal install
% egison-tutorial
Egison Tutorial Version X.X.X (C) 2013-2014 Satoshi Egi
Welcome to Egison Tutorial!
** Information **
We can use a 'Tab' key to complete keywords on the interpreter.
If we type a 'Tab' key after a closed parenthesis, the next closed parenthesis will be completed.
*****************
==============================
List of sections in the tutorial
1: Calculate numbers
2: Basics of functional programming
3: Basics of pattern-matching
4: Pattern-matching against infinite collections
==============================
Choose a section to learn.
(1-4): 4
====================
We can write a pattern-matching against infinite lists even if that has infinite results.
Note that Egison really enumurate all pairs of two natural numbers in the following example.

Examples:
  (take 10 (match-all nats (set integer) [<cons $m <cons $n _>> [m n]]))
====================
>
```
