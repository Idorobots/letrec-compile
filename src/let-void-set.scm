;; The trivial transformation that converts letrec into a let-void-set! construction.

;; This transformation makes use of mutation. It separates the variable creation from the actual value creation,
;; so that all the bound values "see" all of the variables:

;; (letrec ((foo foo-value)
;;          (bar bar-value))
;;   body)

;; =>

;; (let ((foo (void))
;;       (bar (void)))
;;   (set! foo foo-value)
;;   (set! bar bar-value)
;;   body)

(load "utils.scm")

;; A "special" void value that indicates an unbound variable.
(define (void)
  (when #f #f))

(define (let-void-set-conversion expr)
  (let* ((bindings (letrec-bindings expr))
         (vars (bindings-vars bindings)))
    `(let ,(map (lambda (v)
                  (list v '(void)))
                vars)
       ,@(map (lambda (b)
                `(set! ,@b))
              bindings)
       ,(letrec-body expr))))

;; Some examples:

(eval-after-conversion
 let-void-set-conversion
 '(letrec ((foo 'foo-value)
           (bar 'bar-value))
    'body))

(eval-after-conversion
 let-void-set-conversion
 '(letrec () '()))

(eval-after-conversion
 let-void-set-conversion
 '(letrec ((foo 23)
           (bar (+ 5 foo)))
    bar))

(eval-after-conversion
 let-void-set-conversion
 '(letrec ((foo 23)
           (bar (lambda (x) (+ x foo))))
    (bar 5)))

(eval-after-conversion
 let-void-set-conversion
 '(letrec ((foo (lambda (x) (* x 23)))
           (bar (lambda (y) (foo y))))
    (bar 23)))

;; NOTE Never finishes.
(unless #t
  (eval-after-conversion
   let-void-set-conversion
   '(letrec ((foo (lambda () (foo)))) (foo))))

;; NOTE Never finishes.
(unless #t
  (eval-after-conversion
   let-void-set-conversion
   '(letrec ((foo (lambda () (bar)))
             (bar (lambda () (foo))))
      (foo))))

(eval-after-conversion
 let-void-set-conversion
 '(letrec ((a (lambda () (b)))
           (b (lambda () (begin (c) (d))))
           (c (lambda () (a)))
           (d (lambda () (e)))
           (e (lambda () 23)))
    (d)))

(eval-after-conversion
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

(eval-after-conversion
 let-void-set-conversion
 '(letrec ((one (lambda ()
                  (+ 1 (two))))
           (two (lambda ()
                  (+ 2 (three))))
           (three (lambda ()
                    3)))
    (one)))

(time
 (eval-after-conversion
  let-void-set-conversion
  '(letrec ((fib (lambda (n)
                   (if (< n 1)
                       1
                       (+ (fib (- n 1))
                          (fib (- n 2)))))))
     (fib 35))))
