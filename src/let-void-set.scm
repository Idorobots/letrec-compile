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
 letrec-convert
 '(letrec ((even? (lambda (x)
                    (or (zero? x)
                        (odd? (- x 1)))))
           (odd? (lambda (x)
                   (not (even? x)))))
    (list (even? 23)
          (odd? 23)
          (even? 4)
          (odd? 4))))
