# letrec-compile
Figuring out how to compile `letrec` expressions in a simple Lisp dialect.

So far the following methods are implemented:
- Simple let-void-set conversion.
- Fixpoint-based conversion as proposed in Recursion without Circularity.
- SCC reordering with fixpoint conversion.
- SCC reordering with Waddell fixing letrec (with, again, fixpoint conversion in place of the fix expression) as proposed in [Fixing Letrec](https://guenchi.github.io/Scheme/doc/Fixing%20Letrec%20A%20Faithful%20Yet%20Efficient%20Implementation%20of%20Scheme%E2%80%99s%20Recursive%20Binding%20Construct.pdf) and the [other Fixing Letrec](https://guenchi.github.io/Scheme/doc/Fixing%20Letrec%20(reloaded).pdf).
