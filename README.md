# letrec-compile
Figuring out how to compile `letrec` expressions in a simple Lisp dialect.

So far the following methods are implemented:
- Simple let-void-set conversion.
- Fixpoint-based conversion as proposed in Recursion without Circularity.
- SCC reordering with fixpoint conversion.
- SCC reordering with Waddell fixing letrec (with, again, fixpoint conversion in place of the fix expression) as proposed in [Fixing Letrec](https://guenchi.github.io/Scheme/doc/Fixing%20Letrec%20A%20Faithful%20Yet%20Efficient%20Implementation%20of%20Scheme%E2%80%99s%20Recursive%20Binding%20Construct.pdf) and the [other Fixing Letrec](https://guenchi.github.io/Scheme/doc/Fixing%20Letrec%20(reloaded).pdf).
- An SCC-based Waddell-like conversion that also performs cheap assignment conversion where appropriate (makes sense for a dialect of Scheme, where mutation is not available, but still needed to implement letrec).

A benchmark is provided in `test/benchmark.rkt` comparing the performance of the various methods against the Scheme implementation used:

```
$ racket test/benchmark.rkt
Racket:                   cpu time: 489 real time: 490 gc time: 0
let-void-set!:            cpu time: 502 real time: 502 gc time: 0
Fixpoint conversion:      cpu time: 3090 real time: 3099 gc time: 45
SCC conversion:           cpu time: 768 real time: 769 gc time: 6
Fixing letrec:            cpu time: 957 real time: 960 gc time: 14
Fixing letrec conversion: cpu time: 800 real time: 802 gc time: 7
let-ref-fix:              cpu time: 1318 real time: 1320 gc time: 13
Ref conversion:           cpu time: 798 real time: 800 gc time: 7
```
