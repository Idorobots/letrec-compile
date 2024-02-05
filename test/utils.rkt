#lang racket

(require "testing.rkt")
(require "../src/utils.rkt")

;; Some examples:

(is (free-vars '())
    '())

(is (free-vars 'foo)
    '(foo))

(is (free-vars '(list 23 foo))
    '(list foo))

(is (free-vars '(lambda (foo) (list 23 foo)))
    '(list))
