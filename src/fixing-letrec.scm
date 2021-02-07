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
(load "let-void-set.scm")

(define (waddell fix let-void-set expr)
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
                          bindings))
         (lambdas-builder (cond ((empty? lambdas)
                                 identity)
                                ((not (recoursive? lambdas))
                                 (lambda (body)
                                   `(let ,lambdas
                                      ,body)))
                                (else
                                 (lambda (body)
                                   (fix
                                    `(letrec ,lambdas
                                       ,body))))))
         (complex-builder (if (empty? complex)
                              lambdas-builder
                              (lambda (body)
                                (let ((conv (let-void-set `(letrec ,complex
                                                             ,body))))
                                  (list (car conv) (letrec-bindings conv)
                                        (lambdas-builder
                                         (letrec-body conv)))))))
         (simple-builder (if (empty? simple)
                             identity
                             (lambda (body)
                               `(let ,simple
                                  ,body)))))
    (simple-builder
     (complex-builder
      (letrec-body expr)))))

(define (fixing-letrec expr)
  (waddell fixpoint-conversion ;; NOTE Ideally this just creates a (fix bindings body) construct that is later handled efficiently by the closure conversion phase.
           let-void-set-conversion
           expr))

(define (fixing-letrec-conversion expr)
  (scc-reorder (derive-graph expr)
               fixing-letrec
               expr))
