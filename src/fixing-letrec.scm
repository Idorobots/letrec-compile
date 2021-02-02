;; This conversion combines the best of three(?) worlds by using SCC reordering and mutation.

;; It begins much like the SCC-based conversion by reordering bindings and concentrating them into strongly connected components.
;; Afterwards, each SCC is converted according to the following logic:
;; First, partition the bindings into three sets - simple, lambdas and complex.

;; (letrec ((simple 23)
;;          (complex (func simple))
;;          (func (lambda (x) (+ x simple))))
;;   complex)

;; Simple bindings can be pulled out into a wrapping let expression.

;; (let ((simple 23))
;;   (letrec ((complex (func simple))
;;            (func (lambda (x) (+ x simple))))
;;     complex))

;; Lambdas are converted into a fix expression that can be efficiently handled later.

;; (let ((simple 23))
;;   (letrec ((complex (func simple)))
;;     (fix ((func (lambda (x) (+ x simple))))
;;          complex)))

;; Complex bindings are converted using the let-void-set method.

;; (let ((simple 23))
;;   (let ((complex (void)))
;;     (fix ((func (lambda (x) (+ x simple))))
;;          (set! complex (func simple))
;;          complex)))

;; The fix expression can be either handled much like in the fixpoint conversion, or directly during the closure conversion phase by allocating a fat closure for all of these functions.

(load "utils.scm")
(load "scc.scm")
(load "fixpoint.scm")

(define (simple? expr)
  (or (number? expr)
      (string? expr)
      (quote? expr)))

(define (fix bindings body)
  ;; NOTE Ideally this just creates a (fix bindings body) construct that is later handled efficiently by the closure conversion phase.
  (fixpoint-conversion `(letrec ,bindings ,body)))

(define (fixing-letrec expr)
  (let* ((bindings (letrec-bindings expr))
         (simple (filter (lambda (b)
                           (simple? (binding-val b)))
                         bindings))
         (lambdas (filter (lambda (b)
                            (lambda? (binding-val b)))
                          bindings))
         (complex (filter (lambda (b)
                            (not (or (member b simple)
                                     (member b lambdas))))
                          bindings)))
    `(let ,simple
       (let ,(map (lambda (v)
                    (list v '(void)))
                  (bindings-vars complex))
         ,(fix lambdas
               `(begin ,@(map (lambda (b)
                                `(set! ,@b))
                              complex)
                       ,(letrec-body expr)))))))

(define (fixing-letrec-conversion expr)
  (scc-reorder (if (letrec*? expr)
                   (lambda (bindings)
                     (let ((deps (derive-dependencies bindings)))
                       (append deps
                               (filter (lambda (e)
                                         (not (member e deps)))
                                       (derive-ordering bindings)))))
                   derive-dependencies)
               fixing-letrec expr))

;; Some examples:

(eval-after-conversion
 fixing-letrec-conversion
 '(letrec ((foo 'foo-value)
           (bar 'bar-value))
    'body))

(eval-after-conversion
 fixing-letrec-conversion
 '(letrec () '()))

(eval-after-conversion
 fixing-letrec-conversion
 '(letrec ((foo 23)
           (bar (+ 5 foo)))
    bar))

(eval-after-conversion
 fixing-letrec-conversion
 '(letrec ((bar (lambda (x) (+ x foo)))
           (foo 23))
    (bar 5)))

(eval-after-conversion
 fixing-letrec-conversion
 '(letrec* ((bar (lambda (x) (+ x foo)))
            (foo 23))
    (bar 5)))

(eval-after-conversion
 fixing-letrec-conversion
 '(letrec ((foo (lambda (x) (* x 23)))
           (bar (lambda (y) (foo y))))
    (bar 23)))

;; NOTE Never finishes.
(unless #t
  (eval-after-conversion
   fixing-letrec-conversion
   '(letrec ((foo (lambda () (foo)))) (foo))))

;; NOTE Never finishes.
(unless #t
  (eval-after-conversion
   fixing-letrec-conversion
   '(letrec ((foo (lambda () (bar)))
             (bar (lambda () (foo))))
      (foo))))

(eval-after-conversion
 fixing-letrec-conversion
 '(letrec ((a (lambda () (b)))
           (b (lambda () (begin (c) (d))))
           (c (lambda () (a)))
           (d (lambda () (e)))
           (e (lambda () 23)))
    (d)))

(eval-after-conversion
 fixing-letrec-conversion
 '(letrec ((even? (lambda (x)
                    (or (zero? x)
                        (odd? (- x 1)))))
           (odd? (lambda (x)
                   (not (even? x)))))
    (list (even? 23)
          (odd? 23)
          (even? 4)
          (odd? 4))))

(eval-after-conversion
 fixing-letrec-conversion
 '(letrec ((one (lambda ()
                  (+ 1 (two))))
           (two (lambda ()
                  (+ 2 (three))))
           (three (lambda ()
                    3)))
    (one)))

(time
 (eval-after-conversion
  fixing-letrec-conversion
  '(letrec ((fib (lambda (n)
                   (if (< n 1)
                       1
                       (+ (fib (- n 1))
                          (fib (- n 2)))))))
     (fib 35))))
