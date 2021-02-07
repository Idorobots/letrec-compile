(load "../test/testing.scm")
(load "../src/ref.scm")

;; Assignment conversion:

(is (derefy '() '())
    '())

(is (derefy 'foo '())
    'foo)

(is (derefy 'foo '(foo))
    '(deref foo))

(is (derefy '(bar foo) '(foo))
    '(bar (deref foo)))

(is (derefy '(let ((foo foo)) foo) '(foo))
    '(let ((foo foo))
       foo))

(is (derefy '(lambda (foo) foo) '(foo))
    '(lambda (foo) foo))

(is (derefy '(let ((bar foo)) foo) '(foo))
    '(let ((bar (deref foo)))
       (deref foo)))

(is (derefy '(lambda (bar) foo) '(foo))
    '(lambda (bar) (deref foo)))

;; Conversion:

(is (eval-after-conversion
     ref-conversion
     '(letrec ((foo 'foo-value)
               (bar 'bar-value))
        'body))
    'body)

(is (eval-after-conversion
     ref-conversion
     '(letrec () '()))
    '())

(is (eval-after-conversion
     ref-conversion
     '(letrec ((foo 23)
               (bar (+ 5 foo)))
        bar))
    28)

(is (eval-after-conversion
     ref-conversion
     '(letrec ((bar (lambda (x) (+ x foo)))
               (foo (+ 23 5)))
        (bar 5)))
    33)

(is (eval-after-conversion
     ref-conversion
     '(letrec* ((bar (lambda (x) (+ x foo)))
                (foo (+ 23 5)))
        (bar 5)))
    33)

(is (eval-after-conversion
     ref-conversion
     '(letrec ((foo (lambda (x) (* x 23)))
               (bar (lambda (y) (foo y))))
        (bar 23)))
    (* 23 23))

(is (eval-after-conversion
     ref-conversion
     '(letrec ((a (lambda () (b)))
               (b (lambda () (begin (c) (d))))
               (c (lambda () (a)))
               (d (lambda () (e)))
               (e (lambda () 23)))
        (d)))
    23)

(is (eval-after-conversion
     ref-conversion
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
     ref-conversion
     '(letrec ((f (lambda () (even? 5)))
               (even? (lambda (x)(or (zero? x) (odd? (- x 1)))))
               (odd? (lambda (x) (not (even? x))))
               (t (f)))
        t))
    #f)

(is (eval-after-conversion
     ref-conversion
     '(letrec ((one (lambda ()
                      (+ 1 (two))))
               (two (lambda ()
                      (+ 2 (three))))
               (three (lambda ()
                        3)))
        (one)))
    6)

(is (eval-after-conversion
     ref-conversion
     '(letrec ((lazy-23 (cons 23 (lambda () lazy-23)))
               (lazy-take (lambda (list n)
                            (if (zero? n)
                                '()
                                (cons (car list)
                                      (lazy-take ((cdr list))
                                                 (- n 1))))))
               (bar (foldl + 0 (lazy-take lazy-23 5))))
        bar))
    (* 5 23))

;; Can't run these as these produce infinite loops.

(is (ref-conversion
     '(letrec ((foo (lambda () (foo)))) (foo)))
    '(let ((foo (lambda (foo)
                  (lambda ()
                    (let ((foo (foo foo)))
                      (foo))))))
       (let ((foo (foo foo)))
         (foo))))

(is (ref-conversion
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
