;; Some utils.

;; (let bindings body)

(define (let? expr)
  (and (list? expr)
       (equal? (car expr) 'let)))

(define (let-bindings expr)
  (cadr expr))

(define (let-body expr)
  (caddr expr))

;; (letrec bindings body)

(define (letrec? expr)
  (and (list? expr)
       (equal? (car expr) 'letrec)))

(define letrec-bindings let-bindings)

(define letrec-body let-body)

;; (lambda vars body)

(define (lambda? expr)
  (and (list? expr)
       (equal? (car expr) 'lambda)))

(define (lambda-vars expr)
  (cadr expr))

(define (lambda-body expr)
  (caddr expr))

;; ((var val) ...)

(define binding-var car)

(define binding-val cadr)

(define (bindings-vars bindings)
  (map binding-var bindings))

(define (bindings-vals bindings)
  (map binding-val bindings))

(define (eval-after-conversion f expr)
  (display "Expression:") (newline)
  (pretty-print expr) (newline)
  (display "Conversion:") (newline)
  (let ((result (f expr)))
    (pretty-print result) (newline)
    (display "Result:") (newline)
    (pretty-print (eval result)) (newline)))
