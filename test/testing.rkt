#lang racket

(provide (all-defined-out))

;; Some testing utils.

(define (is a b)
  (when (not (equal? a b))
    (error (format "~a not equal to ~a~n" a b)))
  a)
