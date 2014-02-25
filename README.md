## The Egison Totorial

This is a repository for a tutorial program of [Egison](https://github.com/egisatoshi/egison).
Egison is the pattern-matching oriented pure functional programming langauge.
Egison is the world's first programming language that can pattern-match against sets.

For more information, visit [Egison public site](http://www.egison.org).

If you get interested in Egison, please mail to [Satoshi Egi](http://www.egison.org/~egi/) or tweet to [@__Egi](https://twitter.com/__Egi) or [@Egison_Lang](https://twitter.com/Egison_Lang).

### How to compile and run

```
% cabal install
% egison-tutorial
Egison Tutorial for Version X.X.X (C) 2013-2014 Satoshi Egi
http://www.egison.org
Welcome to Egison Tutorial!
==============================
List of sections in the tutorial
1: Calculate numbers
2: Basics of functional programming
3: Define your own functions
4: Basic of pattern-matching
5: Pattern-matching against infinite collections
6: Writing scripts in Egison
==============================
Choose a section to learn.
(1-6): 5
====================
We can write a pattern-matching against infinite lists even if that has infinite results.
Note that Egison really enumurate all pairs of two natural numbers in the following example.

Examples:
  (take 10 (match-all nats (set integer) [<cons $m <cons $n _>> [m n]]))
====================
>
```
