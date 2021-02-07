(load "../test/testing.scm")
(load "../src/let-void-set.scm")
(load "../src/fixpoint.scm")
(load "../src/scc.scm")
(load "../src/fixing-letrec.scm")
(load "../src/ref.scm")

(define benchmark-expr
  '(letrec ((expected 63245986)
            (one 1)
            (two (+ one one))
            (fib (lambda (n)
                   (if (< n one)
                       one
                       (+ (fib (- n one))
                          (fib (- n two))))))
            (result (fib 37)))
     (is result 63245986))) ;; FIXME SCC currently cuts the expected var out.

;; Some timings:

(time (eval-after-conversion identity benchmark-expr))

(time (eval-after-conversion let-void-set-conversion benchmark-expr))

(time (eval-after-conversion fixpoint-conversion benchmark-expr))

(time (eval-after-conversion scc-conversion benchmark-expr))

(time (eval-after-conversion fixing-letrec benchmark-expr))

(time (eval-after-conversion fixing-letrec-conversion benchmark-expr))

(time (eval-after-conversion let-ref-fix benchmark-expr))

(time (eval-after-conversion ref-conversion benchmark-expr))
