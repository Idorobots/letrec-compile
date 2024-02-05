#lang racket

(require "testing.rkt")
(require "../src/let-void-set.rkt")
(require "../src/fixpoint.rkt")
(require "../src/scc.rkt")
(require "../src/fixing-letrec.rkt")
(require "../src/ref.rkt")

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
     (is result expected)))

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

(define (run-bench f expr)
  (time (eval (f expr) ns))
  (void))

;; Some timings:

(collect-garbage 'major)
(display "Racket:                   ")
(run-bench identity benchmark-expr)

(collect-garbage 'major)
(display "let-void-set!:            ")
(run-bench let-void-set-conversion benchmark-expr)

(collect-garbage 'major)
(display "Fixpoint conversion:      ")
(run-bench fixpoint-conversion benchmark-expr)

(collect-garbage 'major)
(display "SCC conversion:           ")
(run-bench scc-conversion benchmark-expr)

(collect-garbage 'major)
(display "Fixing letrec:            ")
(run-bench fixing-letrec benchmark-expr)

(collect-garbage 'major)
(display "Fixing letrec conversion: ")
(run-bench fixing-letrec-conversion benchmark-expr)

(collect-garbage 'major)
(display "let-ref-fix:              ")
(run-bench let-ref-fix benchmark-expr)

(collect-garbage 'major)
(display "Ref conversion:           ")
(run-bench ref-conversion benchmark-expr)
