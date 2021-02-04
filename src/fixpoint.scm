;; This conversion uses the generalized Y combinator in order to implement recursion without circularity or mutation.

;; The conversion is performed in multiple steps, starting with the input code:

;; (letrec ((even? (lambda (x)
;;                   (or (zero? x)
;;                       (odd? (- x 1)))))
;;          (odd? (lambda (x)
;;                  (not (even? x)))))
;;   (list (even? 1)
;;         (odd? 2)
;;         (even? 2)
;;         (odd? 3)))

;; First add all the letrec binding vars as extra parameters to the lambdas:

;; (let ((even?+ (lambda (x even? odd?)
;;                 (or (zero? x)
;;                     (odd? (- x 1)))))
;;       (odd?+ (lambda (x even? odd?)
;;                (not (even? x)))))
;;   (list (even? 1)
;;         (odd? 2)
;;         (even? 2)
;;         (odd? 3)))

;; Abstract these into a separate lambda that returns the original function:

;; (let ((even?+ (lambda (even? odd?)
;;                 (lambda (x)
;;                   (or (zero? x)
;;                       (odd? (- x 1))))))
;;       (odd?+ (lambda (even? odd?)
;;                (lambda (x)
;;                  (not (even? x))))))
;;   (list (even? 1)
;;         (odd? 2)
;;         (even? 2)
;;         (odd? 3)))

;; Redefine the original functions within the body of the outer lambda:

;; (let ((even?+ (lambda (even?* odd?*)
;;                 (let ((even? (even?* even?* odd?*))
;;                       (odd? (odd?* even?* odd?*)))
;;                   (lambda (x)
;;                     (or (zero? x)
;;                         (odd? (- x 1)))))))
;;       (odd?+ (lambda (even?* odd?*)
;;                (let ((even? (even?* even?* odd?*))
;;                      (odd? (odd?* even?* odd?*)))
;;                  (lambda (x)
;;                    (not (even? x)))))))
;;   (list (even? 1)
;;         (odd? 2)
;;         (even? 2)
;;         (odd? 3)))

;; Redefine the original functions within the letrec body:

;; (let ((even?+ (lambda (even?* odd?*)
;;                 (lambda (x)
;;                   (let ((even? (even?* even?* odd?*))
;;                         (odd? (odd?* even?* odd?*)))
;;                     (or (zero? x)
;;                         (odd? (- x 1)))))))
;;       (odd?+ (lambda (even?* odd?*)
;;                (lambda (x)
;;                  (let ((even? (even?* even?* odd?*))
;;                        (odd? (odd?* even?* odd?*)))
;;                    (not (even? x)))))))
;;   (let ((even? (even?+ even?+ odd?+))
;;         (odd? (odd?+ even?+ odd?+)))
;;     (list (even? 1)
;;           (odd? 2)
;;           (even? 2)
;;           (odd? 3))))

;; Prune the unused values:

;; (let ((even? (lambda (even?* odd?*)
;;                (lambda (x)
;;                  (let ((odd? (odd?* even?* odd?*)))
;;                    (or (zero? x)                       ;; Same as original.
;;                        (odd? (- x 1)))))))
;;       (odd? (lambda (even?* odd?*)
;;               (lambda (x)
;;                 (let ((even? (even?* even?* odd?*)))
;;                   (not (even? x)))))))                 ;; Same as original.
;;   (let ((even? (even?+ even?+ odd?+))
;;         (odd? (odd?+ even?+ odd?+)))
;;     (list (even? 1)                                    ;; Same as original.
;;           (odd? 2)
;;           (even? 2)
;;           (odd? 3))))

;; Analogically, the conversion can be applied to non-lambda variables by thunkifying them first and then forcing shortly before usage:

;; (letrec ((a (cons 1 b))
;;          (b (cons 2 a)))
;;   (cons a b))

;; Thunkify the values:

;; (letrec ((a+ (lambda () (cons 1 b)))
;;          (b+ (lambda () (cons 2 a))))
;;   (cons a b))

;; Introduce the extra parameters:

;; (let ((a+ (lambda (a b) (cons 1 b)))
;;       (b+ (lambda (a b) (cons 2 a))))
;;   (cons a b))

;; Abstract these extra parameters away into an outer function:

;; (let ((a+ (lambda (a* b*) (lambda () (cons 1 b))))
;;       (b+ (lambda (a* b*) (lambda () (cons 2 a)))))
;;   (cons a b))

;; Redefine the original values within the inner lambdas and force their execution:

;; (let ((a+ (lambda (a* b*)
;;            (lambda ()
;;              (let ((a ((a* a* b*)))
;;                    (b ((b* a* b*))))
;;                (cons 1 b)))))
;;       (b+ (lambda (a* b*)
;;            (lambda ()
;;              (let ((a ((a* a* b*)))
;;                    (b ((b* a* b*)))))
;;              (cons 2 a)))))
;;   (cons a b))

;; Redefine the original values within the letrec body ensuring to force their execution:

;; (let ((a+ (lambda (a* b*)
;;             (lambda ()
;;               (let ((a ((a* a* b*)))
;;                     (b ((b* a* b*))))
;;                 (cons 1 b)))))                              ;; Same as original
;;       (b+ (lambda (a* b*)
;;             (lambda ()
;;               (let ((a ((a* a* b*)))
;;                     (b ((b* a* b*))))
;;                 (cons 2 a))))))                             ;; Same as original
;;   (let ((a ((a+ a+ b+)))
;;         (b ((b+ a+ b)+)))
;;     (cons a b)))                                            ;; Same as original

(load "utils.scm")

(define (rebound-vars vars)
  (map (lambda (v)
         (list v (cons v vars)))
       vars))

(define (filter-unused bindings free-vars)
  (filter (lambda (b)
            (member (binding-var b) free-vars))
          bindings))

(define (rewrapped-lambdas vars lambdas postprocess)
  (map (lambda (v l)
         (list v
               (let* ((body (lambda-body l))
                      (free-vars (free-vars body)))
                 `(lambda ,vars
                    (lambda ,(lambda-vars l)
                      (let ,(postprocess (filter-unused (rebound-vars vars) free-vars))
                        ,body))))))
       vars
       lambdas))

(define (filter-bindings pred? bindings)
  (cond ((empty? bindings)
         '())
        ((pred? (cadar bindings))
         (cons (car bindings)
               (filter-bindings pred? (cdr bindings))))
        (else
         (filter-bindings pred? (cdr bindings)))))

(define (thunkify only-these)
  (lambda (bindings)
    (map (lambda (b)
           (if (member (car b) only-these)
               (list (car b)
                     `(lambda ()
                        ,(cadr b)))
               b))
         bindings)))

(define (forcify only-these)
  (lambda (bindings)
    (map (lambda (b)
           (if (member (car b) only-these)
               (list (car b)
                     `(,(cadr b)))
               b))
         bindings)))

(define (fixpoint-conversion expr)
  (let ((bindings (letrec-bindings expr))
        (body (letrec-body expr)))
    (if (empty? bindings)
        body
        (let* ((lambda-bindings (filter-bindings lambda? bindings))
               (lambda-vars (bindings-vars lambda-bindings))
               (rest-bindings (filter-bindings (compose not lambda?) bindings))
               (rest-vars (bindings-vars rest-bindings))
               (vars (bindings-vars bindings))
               (body-free-vars (free-vars body)))
          `(let ,(rewrapped-lambdas vars
                                    (bindings-vals ((thunkify rest-vars) bindings))
                                    (forcify rest-vars))
             (let ,((forcify rest-vars)
                    (filter-unused (rebound-vars vars) body-free-vars))
               ,body))))))

;; Some examples:

(eval-after-conversion
 fixpoint-conversion
 '(letrec ((foo 'foo-value)
           (bar 'bar-value))
    'body))

(eval-after-conversion
 fixpoint-conversion
 '(letrec () '()))

(eval-after-conversion
 fixpoint-conversion
 '(letrec ((foo 23)
           (bar (+ 5 foo)))
    bar))

(eval-after-conversion
 fixpoint-conversion
 '(letrec ((bar (lambda (x) (+ x foo)))
           (foo (+ 23 5)))
    (bar 5)))

(eval-after-conversion
 fixpoint-conversion
 '(letrec* ((bar (lambda (x) (+ x foo)))
            (foo (+ 23 5)))
    (bar 5)))

(eval-after-conversion
 fixpoint-conversion
 '(letrec ((foo (lambda (x) (* x 23)))
           (bar (lambda (y) (foo y))))
    (bar 23)))

;; NOTE Never finishes.
(unless #t
  (eval-after-conversion
   fixpoint-conversion
   '(letrec ((foo (lambda () (foo)))) (foo))))

;; NOTE Never finishes.
(unless #t
  (eval-after-conversion
   fixpoint-conversion
   '(letrec ((foo (lambda () (bar)))
             (bar (lambda () (foo))))
      (foo))))

(eval-after-conversion
 fixpoint-conversion
 '(letrec ((a (lambda () (b)))
           (b (lambda () (begin (c) (d))))
           (c (lambda () (a)))
           (d (lambda () (e)))
           (e (lambda () 23)))
    (d)))

(eval-after-conversion
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

(eval-after-conversion
 fixpoint-conversion
 '(letrec ((one (lambda ()
                  (+ 1 (two))))
           (two (lambda ()
                  (+ 2 (three))))
           (three (lambda ()
                    3)))
    (one)))

(time
 (eval-after-conversion
  fixpoint-conversion
  '(letrec ((fib (lambda (n)
                   (if (< n 1)
                       1
                       (+ (fib (- n 1))
                          (fib (- n 2)))))))
     (fib 35))))
