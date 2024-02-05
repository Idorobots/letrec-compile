#lang racket

(require "testing.rkt")
(require "../src/let-void-set.rkt")
(require "../src/utils.rkt")

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

;; Conversion:

(is (eval-after-conversion
     ns
     let-void-set-conversion
     '(letrec ((foo 'foo-value)
               (bar 'bar-value))
        'body))
    'body)

(is (eval-after-conversion
     ns
     let-void-set-conversion
     '(letrec () '()))
    '())

(is (eval-after-conversion
     ns
     let-void-set-conversion
     '(letrec ((foo 23)
               (bar (+ 5 foo)))
        bar))
    28)

(is (eval-after-conversion
     ns
     let-void-set-conversion
     '(letrec ((bar (lambda (x) (+ x foo)))
               (foo (+ 23 5)))
        (bar 5)))
    33)

(is (eval-after-conversion
     ns
     let-void-set-conversion
     '(letrec* ((bar (lambda (x) (+ x foo)))
                (foo (+ 23 5)))
        (bar 5)))
    33)

(is (eval-after-conversion
     ns
     let-void-set-conversion
     '(letrec ((foo (lambda (x) (* x 23)))
               (bar (lambda (y) (foo y))))
        (bar 23)))
    (* 23 23))

(is (eval-after-conversion
     ns
     let-void-set-conversion
     '(letrec ((a (lambda () (b)))
               (b (lambda () (begin (c) (d))))
               (c (lambda () (a)))
               (d (lambda () (e)))
               (e (lambda () 23)))
        (d)))
    23)

(is (eval-after-conversion
     ns
     let-void-set-conversion
     '(letrec ((even? (lambda (x)
                        (or (zero? x)
                            (odd? (- x 1)))))
               (odd? (lambda (x)
                       (not (even? x)))))
        (list (even? 23)
              (odd? 23)
              (even? 4)
              (odd? 4))))
    (list #f #t #t #f))

(is (eval-after-conversion
     ns
     let-void-set-conversion
     '(letrec ((f (lambda () (even? 5)))
               (even? (lambda (x)(or (zero? x) (odd? (- x 1)))))
               (odd? (lambda (x) (not (even? x))))
               (t (f)))
        t))
    #f)

(is (eval-after-conversion
     ns
     let-void-set-conversion
     '(letrec ((one (lambda ()
                      (+ 1 (two))))
               (two (lambda ()
                      (+ 2 (three))))
               (three (lambda ()
                        3)))
        (one)))
    6)

(is (eval-after-conversion
     ns
     let-void-set-conversion
     '(letrec ((lazy-23 (cons 23 (lambda () lazy-23)))
               (lazy-take (lambda (list n)
                            (if (zero? n)
                                '()
                                (cons (car list)
                                      (lazy-take ((cdr list))
                                                 (- n 1))))))
               (bar (foldr + 0 (lazy-take lazy-23 5))))
        bar))
    (* 5 23))

;; Some timings:

(is (time
     (eval-after-conversion
      ns
      let-void-set-conversion
      '(letrec ((fib (lambda (n)
                       (if (< n 1)
                           1
                           (+ (fib (- n 1))
                              (fib (- n 2)))))))
         (fib 35))))
    24157817)

;; Can't run these as these produce infinite loops.

(is (let-void-set-conversion
     '(letrec ((foo (lambda () (foo)))) (foo)))
    '(let ((foo (void)))
       (begin
         (set! foo (lambda () (foo)))
         (foo))))

(is (let-void-set-conversion
     '(letrec ((foo (lambda () (bar)))
               (bar (lambda () (foo))))
        (foo)))
    '(let ((foo (void))
           (bar (void)))
       (begin
         (set! foo (lambda () (bar)))
         (set! bar (lambda () (foo)))
         (foo))))
