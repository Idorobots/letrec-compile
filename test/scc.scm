(load "../test/testing.scm")
(load "../src/scc.scm")

;; SCC

;;   a
;;  / \
;; b---c
;; |
;; d---e

(is (scc '(a b c d e)
         '((a b)
           (b d)
           (b c)
           (c a)
           (d e)
           (e)))
    '((e) (d) (a c b)))

(is (scc '(a b c d e)
         '((c a)
           (a b)
           (b c)
           (b d)
           (d e)
           (e)))
    '((e) (d) (a c b)))

(is (scc '(a b c d e)
         '((d e)
           (c a)
           (a b)
           (b c)
           (b d)
           (e)))
    '((e) (d) (a c b)))

;;   a
;;  / \
;; b---c
;;
;; d---e

(is (scc '(a b c d e)
         '((a b)
           (b c)
           (c a)
           (d e)
           (e)))
    '((a c b) (e) (d)))

;;   a---b---c---d---e

(is (scc '(a b c d e)
         '((a b)
           (b c)
           (c d)
           (d e)
           (e)))
    '((e) (d) (c) (b) (a)))

;;   a---b---c---d---e
;;    \_____________/

(is (scc '(a b c d e)
         '((a b)
           (b c)
           (c d)
           (d e)
           (e a)))
    '((a e d c b)))

;; Conversion:

(is (eval-after-conversion
     scc-conversion
     '(letrec ((foo 'foo-value)
               (bar 'bar-value))
        'body))
    'body)

(is (eval-after-conversion
     scc-conversion
     '(letrec () '()))
    '())

(is (eval-after-conversion
     scc-conversion
     '(letrec ((foo 23)
               (bar (+ 5 foo)))
        bar))
    28)

(is (eval-after-conversion
     scc-conversion
     '(letrec ((bar (lambda (x) (+ x foo)))
               (foo (+ 23 5)))
        (bar 5)))
    33)

(is (eval-after-conversion
     scc-conversion
     '(letrec* ((bar (lambda (x) (+ x foo)))
                (foo (+ 23 5)))
        (bar 5)))
    33)

(is (eval-after-conversion
     scc-conversion
     '(letrec ((foo (lambda (x) (* x 23)))
               (bar (lambda (y) (foo y))))
        (bar 23)))
    (* 23 23))

(is (eval-after-conversion
     scc-conversion
     '(letrec ((a (lambda () (b)))
               (b (lambda () (begin (c) (d))))
               (c (lambda () (a)))
               (d (lambda () (e)))
               (e (lambda () 23)))
        (d)))
    23)

(is (eval-after-conversion
     scc-conversion
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
     scc-conversion
     '(letrec ((f (lambda () (even? 5)))
               (even? (lambda (x)(or (zero? x) (odd? (- x 1)))))
               (odd? (lambda (x) (not (even? x))))
               (t (f)))
        t))
    #f)

(is (eval-after-conversion
     scc-conversion
     '(letrec ((one (lambda ()
                      (+ 1 (two))))
               (two (lambda ()
                      (+ 2 (three))))
               (three (lambda ()
                        3)))
        (one)))
    6)

;; Can't run these as these produce infinite loops.

(is (scc-conversion
     '(letrec ((lazy-23 (cons 23 (lambda () lazy-23)))
               (lazy-take (lambda (list n)
                            (if (zero? n)
                                '()
                                (cons (car list)
                                      (lazy-take ((cdr list))
                                                 (- n 1))))))
               (bar (foldl + 0 (lazy-take lazy-23 5))))
        bar))
    '(let ((lazy-23 (lambda (lazy-23)
                      (lambda ()
                        (let ((lazy-23 ((lazy-23 lazy-23))))
                          (cons 23 (lambda () lazy-23)))))))
       (let ((lazy-23 ((lazy-23 lazy-23))))
         (let ((lazy-take (lambda (lazy-take)
                            (lambda (list n)
                              (let ((lazy-take (lazy-take lazy-take)))
                                (if (zero? n)
                                    (quote ())
                                    (cons (car list)
                                          (lazy-take ((cdr list)) (- n 1)))))))))
           (let ((lazy-take (lazy-take lazy-take)))
             (let ((bar (foldl + 0 (lazy-take lazy-23 5))))
               bar))))))

(is (scc-conversion
     '(letrec ((foo (lambda () (foo)))) (foo)))
    '(let ((foo (lambda (foo)
                  (lambda ()
                    (let ((foo (foo foo)))
                      (foo))))))
       (let ((foo (foo foo)))
         (foo))))

(is (scc-conversion
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
