#lang racket

(require "testing.rkt")
(require "../src/fixpoint.rkt")
(require "../src/utils.rkt")

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

;; Conversion:

(is (eval-after-conversion
     ns
     fixpoint-conversion
     '(letrec ((foo 'foo-value)
               (bar 'bar-value))
        'body))
    'body)

(is (eval-after-conversion
     ns
     fixpoint-conversion
     '(letrec () '()))
    '())

(is (eval-after-conversion
     ns
     fixpoint-conversion
     '(letrec ((foo 23)
               (bar (+ 5 foo)))
        bar))
    28)

(is (eval-after-conversion
     ns
     fixpoint-conversion
     '(letrec ((bar (lambda (x) (+ x foo)))
               (foo (+ 23 5)))
        (bar 5)))
    33)

(is (eval-after-conversion
     ns
     fixpoint-conversion
     '(letrec* ((bar (lambda (x) (+ x foo)))
                (foo (+ 23 5)))
        (bar 5)))
    33)

(is (eval-after-conversion
     ns
     fixpoint-conversion
     '(letrec ((foo (lambda (x) (* x 23)))
               (bar (lambda (y) (foo y))))
        (bar 23)))
    (* 23 23))

(is (eval-after-conversion
     ns
     fixpoint-conversion
     '(letrec ((a (lambda () (b)))
               (b (lambda () (begin (c) (d))))
               (c (lambda () (a)))
               (d (lambda () (e)))
               (e (lambda () 23)))
        (d)))
    23)

(is (eval-after-conversion
     ns
     fixpoint-conversion
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
     fixpoint-conversion
     '(letrec ((f (lambda () (even? 5)))
               (even? (lambda (x)(or (zero? x) (odd? (- x 1)))))
               (odd? (lambda (x) (not (even? x))))
               (t (f)))
        t))
    #f)

(is (eval-after-conversion
     ns
     fixpoint-conversion
     '(letrec ((one (lambda ()
                      (+ 1 (two))))
               (two (lambda ()
                      (+ 2 (three))))
               (three (lambda ()
                        3)))
        (one)))
    6)

;; Some timings:

(is (time
     (eval-after-conversion
      ns
      fixpoint-conversion
      '(letrec ((fib (lambda (n)
                       (if (< n 1)
                           1
                           (+ (fib (- n 1))
                              (fib (- n 2)))))))
         (fib 35))))
    24157817)

;; Can't run these as these produce infinite loops.

(is (fixpoint-conversion
     '(letrec ((lazy-23 (cons 23 (lambda () lazy-23)))
               (lazy-take (lambda (list n)
                            (if (zero? n)
                                '()
                                (cons (car list)
                                      (lazy-take ((cdr list))
                                                 (- n 1))))))
               (bar (foldl + 0 (lazy-take lazy-23 5))))
        bar))
    '(let ((lazy-23 (lambda (lazy-23 lazy-take bar)
                      (lambda ()
                        (let ((lazy-23 ((lazy-23 lazy-23 lazy-take bar))))
                          (cons 23 (lambda () lazy-23))))))
           (lazy-take (lambda (lazy-23 lazy-take bar)
                        (lambda (list n)
                          (let ((lazy-take (lazy-take lazy-23 lazy-take bar)))
                            (if (zero? n)
                                (quote ())
                                (cons (car list)
                                      (lazy-take ((cdr list)) (- n 1))))))))
           (bar (lambda (lazy-23 lazy-take bar)
                  (lambda ()
                    (let ((lazy-take (lazy-take lazy-23 lazy-take bar))
                          (lazy-23 ((lazy-23 lazy-23 lazy-take bar))))
                      (foldl + 0 (lazy-take lazy-23 5)))))))
       (let ((bar ((bar lazy-23 lazy-take bar))))
         bar)))

(is (fixpoint-conversion
     '(letrec ((foo (lambda () (foo)))) (foo)))
    '(let ((foo (lambda (foo)
                  (lambda ()
                    (let ((foo (foo foo)))
                      (foo))))))
       (let ((foo (foo foo)))
         (foo))))

(is (fixpoint-conversion
     '(letrec ((foo (lambda () (bar)))
               (bar (lambda () (foo))))
        (foo)))
    '(let ((foo (lambda (foo bar)
                  (lambda ()
                    (let ((bar (bar foo bar)))
                      (bar)))))
           (bar (lambda (foo bar)
                  (lambda ()
                    (let ((foo (foo foo bar)))
                      (foo))))))
       (let ((foo (foo foo bar)))
         (foo))))
