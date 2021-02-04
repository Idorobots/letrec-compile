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
         (vars (bindings-vars bindings))
         (body (letrec-body expr))
         (voids (map (lambda (v)
                       (list v '(void)))
                     vars))
         (setters (map (lambda (b)
                         `(set! ,@b))
                       bindings)))
    (if (empty? setters)
        body
        `(let ,voids
           (begin ,@setters
                  ,body)))))
