;; Much like fixpoint mixed with let-void-set except it relies on boxing instead of thunking. Also SCC.

(load "utils.scm")
(load "fixpoint.scm")
(load "scc.scm")
(load "fixing-letrec.scm")

(define (ref)
  (vector '()))

(define (assign! ref value)
  (vector-set! ref 0 value)
  value)

(define (deref ref)
  (vector-ref ref 0))

(define (derefy expr refs)
  (cond ((empty? refs)
         expr)
        ((simple? expr)
         expr)
        ((and (symbol? expr)
              (member expr refs))
         `(deref ,expr))
        ((or (let? expr)
             (letrec? expr)
             (letrec*? expr))
         (let* ((bindings (let-bindings expr))
                (body (let-body expr))
                (vars (bindings-vars bindings))
                (unbound-refs (filter (lambda (r)
                                        (not (member r vars)))
                                      refs))
                (derefied-bindings (map (lambda (b)
                                          (list (car b)
                                                (derefy (cadr b) unbound-refs)))
                                        bindings)))
           `(,(car expr) ,derefied-bindings
             ,(derefy body unbound-refs))))
        ((lambda? expr)
         (let ((vars (lambda-vars expr)))
           `(lambda ,vars
            ,(derefy (lambda-body expr)
                     (filter (lambda (r)
                               (not (member r vars)))
                             refs)))))
        ((pair? expr)
         (cons (derefy (car expr) refs)
               (derefy (cdr expr) refs)))
        (else expr)))

(define (let-ref-fix expr)
  (let ((bindings (letrec-bindings expr))
        (body (letrec-body expr)))
    (if (empty? bindings)
        body
        (let* ((lambda-bindings (filter-bindings lambda? bindings))
               (lambda-vars (bindings-vars lambda-bindings))
               (rest-bindings (filter-bindings (compose not lambda?) bindings))
               (rest-vars (bindings-vars rest-bindings))
               (vars (bindings-vars bindings))
               (rebound (rebound-vars lambda-vars lambda-vars))
               (body-free-vars (append (free-vars body)
                                       (free-vars (bindings-vals rest-bindings))))
               (let-builder (lambda (bindings body)
                              (if (empty? bindings)
                                  body
                                  `(let ,bindings
                                     ,body))))
               (refs (map (lambda (v)
                            (list v '(ref)))
                          rest-vars))
               (setters (map (lambda (b)
                               `(assign! ,(car b) ,(derefy (cadr b) rest-vars)))
                             rest-bindings))
               (body (derefy body rest-vars)))
          (let-builder refs
                       (let-builder (rewrapped-lambdas lambda-vars
                                                       (map (lambda (v)
                                                              (derefy v rest-vars))
                                                            (bindings-vals lambda-bindings))
                                                       rebound)
                                    (let-builder (filter-unused rebound body-free-vars)
                                                 (if (empty? setters)
                                                     body
                                                     `(begin ,@setters
                                                             ,body)))))))))

(define (ref-conversion expr)
  (scc-reorder (derive-graph expr)
               (lambda (expr)
                 (waddell let-ref-fix
                          let-ref-fix
                          expr))
               expr))
