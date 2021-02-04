;; This conversion uses topological sort on a strongly-connected components DAG of the input letrec in order to "untangle" the recursive definitions as much as possible.

;; This method levredges the fact that binding sets usually are sparsly connected by recursion, meaning they can be reordered to construct much smaller sets of bindings that can be handled separately.
;; This is achieved by using a strongly connected components (SCC) algorithm, such as the Kosaraju's or Tarjan's algorithm, to first partition the graph into smaller chunks and then topologically sort them to preserve variable dependencies.
;; For instance, the following letrec:

;; (letrec ((a (lambda () (b)))
;;          (b (lambda () (c) (d)))
;;          (c (lambda () (a)))
;;          (d (lambda () (e)))
;;          (e (lambda () 23))
;;   (d))

;; ...results in the following graph:

;;   a
;;  / \
;; b---c
;; |
;; d---e

;; ...and the sorted strongly connected components:

;; '((e) (d) (a b c))

;; This in turn allows us to rewrite the original letrec as:

;; (let ((e (lambda () 23)))
;;   (let (d (lambda () (e)))
;;     (letrec ((a (lambda () (b)))
;;              (b (lambda () (c) (d)))
;;              (c (lambda () (a))))
;;       (d))))

;; ...which is considerably simpler to compile and optimize.

(load "utils.scm")
(load "fixpoint.scm")

;; ((a b) (a c) (b c) (c a)) -> ((b a) (c a) (c b) (a c))
(define (transpose g)
  (map reverse (filter (lambda (e)
                         (> (length e) 1))
                       g)))

(define (edges v g)
  (map cadr
       (filter (lambda (e)
                 (and (> (length e) 1)
                      (equal? v (car e))))
               g)))

(define (visit v visited l graph)
  (let ((edges (edges v graph)))
    (if (member v visited) ;; FIXME Could be quicker.
        (list visited l)
        (let* ((result (foldl (lambda (e acc)
                                (visit e (car acc) (cadr acc) graph))
                              (list (cons v visited) l)
                              edges))
               (new-visited (car result))
               (new-l (cadr result)))
          (list new-visited
                (cons v new-l))))))

(define (assign v root assigned scc t-graph)
  (let ((edges (edges v t-graph)))
    (if (member v assigned) ;; FIXME Could be quicker.
        (list assigned scc)
        (foldl (lambda (e acc)
                 (assign e
                         root
                         (car acc)
                         (cadr acc)
                         t-graph))
               (list (cons v assigned)
                     (cons (list root v) scc))
               edges))))

(define (group scc)
  (map cdr
       (foldr (lambda (e acc)
                (cond ((empty? acc) (list e))
                      ((equal? (car e)
                               (caar acc))
                       (cons (append (car acc) (cdr e))
                             (cdr acc)))
                      (else
                       (cons e acc))))
              '()
              scc)))

;; SCC according to the Kosaraju's algorithm:

(define (scc graph)
  (let* ((verts (map car graph))
         (visited (foldl (lambda (v acc)
                           (visit v (car acc) (cadr acc) graph))
                         (list '() '())
                         verts))
         (postorder (cadr visited))
         (t-graph (transpose graph))
         (assigned (foldl (lambda (v acc)
                            (assign v v (car acc) (cadr acc) t-graph))
                          (list '() '())
                          postorder))
         (scc (cadr assigned)))
    (group scc)))

;; Some tests:

;;   a
;;  / \
;; b---c
;; |
;; d---e
(scc '((a b)
       (b d)
       (b c)
       (c a)
       (d e)
       (e)))
;; '((a b c) (d) (e))

(scc '((c a)
       (a b)
       (b c)
       (b d)
       (d e)
       (e)))
;; '((a b c) (d) (e))

(scc '((d e)
       (c a)
       (a b)
       (b c)
       (b d)
       (e)))
;; '((a b c) (d) (e))

;;   a
;;  / \
;; b---c
;;
;; d---e
(scc '((a b)
       (b c)
       (c a)
       (d e)
       (e)))
;; '((d) (e) (a b c))

;;   a---b---c---d---e
(scc '((a b)
       (b c)
       (c d)
       (d e)
       (e)))
;; '((a) (b) (c) (d) (e))

;;   a---b---c---d---e
;;    \_____________/
(scc '((a b)
       (b c)
       (c d)
       (d e)
       (e a)))
;; '((a b c d e))

;; The actual conversion:

(define (recoursive? bindings)
  (or (> (length bindings) 1)
      (member (binding-var (car bindings))
              (free-vars (binding-val (car bindings))))))

(define (reorder-bindings fixer bindings body scc)
  (foldr (lambda (component acc)
           (let ((bs (filter (lambda (b)
                               (member (binding-var b)
                                       component))
                             bindings)))
             (if (recoursive? bs)
                 (fixer
                  `(letrec ,bs
                     ,acc))
                 `(let ,bs
                    ,acc))))
         body
         scc))

(define (derive-dependencies bindings)
  (let ((vars (bindings-vars bindings)))
    (foldl append
           '()
           (map (lambda (b)
                  (map (lambda (e)
                         (list (binding-var b) e))
                       (filter (lambda (v)
                                 (member v vars))
                               (free-vars (binding-val b)))))
                bindings))))

(define (derive-ordering bindings)
  (let ((vars (bindings-vars bindings)))
    (if (not (empty? vars))
        (map list
             (cdr vars)
             (reverse (cdr (reverse vars))))
        '())))

(define (scc-reorder deriver fixer expr)
  (let* ((bindings (letrec-bindings expr))
         (dep-graph (deriver bindings))
         (scc (scc dep-graph)))
    (reorder-bindings fixer bindings (letrec-body expr) scc)))

(define (derive-graph expr)
  (if (letrec*? expr)
      (lambda (bindings)
        (let ((deps (derive-dependencies bindings)))
          (append deps
                  (filter (lambda (e)
                            (not (member e deps)))
                          (derive-ordering bindings)))))
      derive-dependencies))

(define (scc-conversion expr)
  (scc-reorder (derive-graph expr)
               fixpoint-conversion
               expr))

;; Some examples:

(eval-after-conversion
 scc-conversion
 '(letrec ((foo 'foo-value)
           (bar 'bar-value))
    'body))

(eval-after-conversion
 scc-conversion
 '(letrec () '()))

(eval-after-conversion
 scc-conversion
 '(letrec ((foo 23)
           (bar (+ 5 foo)))
    bar))

(eval-after-conversion
 scc-conversion
 '(letrec ((bar (lambda (x) (+ x foo)))
           (foo 23))
    (bar 5)))

(eval-after-conversion
 scc-conversion
 '(letrec* ((bar (lambda (x) (+ x foo)))
            (foo 23))
    (bar 5)))

(eval-after-conversion
 scc-conversion
 '(letrec ((foo (lambda (x) (* x 23)))
           (bar (lambda (y) (foo y))))
    (bar 23)))

;; NOTE Never finishes.
(unless #t
  (eval-after-conversion
   scc-conversion
   '(letrec ((foo (lambda () (foo)))) (foo))))

;; NOTE Never finishes.
(unless #t
  (eval-after-conversion
   scc-conversion
   '(letrec ((foo (lambda () (bar)))
             (bar (lambda () (foo))))
      (foo))))

(eval-after-conversion
 scc-conversion
 '(letrec ((a (lambda () (b)))
           (b (lambda () (begin (c) (d))))
           (c (lambda () (a)))
           (d (lambda () (e)))
           (e (lambda () 23)))
    (d)))

(eval-after-conversion
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

(eval-after-conversion
 scc-conversion
 '(letrec ((f (lambda () (even? 5)))
           (even? (lambda (x)(or (zero? x) (odd? (- x 1)))))
           (odd? (lambda (x) (not (even? x))))
           (t (f)))
    t))

(eval-after-conversion
 scc-conversion
 '(letrec ((one (lambda ()
                  (+ 1 (two))))
           (two (lambda ()
                  (+ 2 (three))))
           (three (lambda ()
                    3)))
    (one)))

(time
 (eval-after-conversion
  scc-conversion
  '(letrec ((fib (lambda (n)
                   (if (< n 1)
                       1
                       (+ (fib (- n 1))
                          (fib (- n 2)))))))
     (fib 35))))
