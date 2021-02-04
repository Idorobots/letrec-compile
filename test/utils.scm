(load "../test/testing.scm")
(load "../src/utils.scm")

;; Some examples:

(is (free-vars '())
    '())

(is (free-vars 'foo)
    '(foo))

(is (free-vars '(list 23 foo))
    '(list foo))

(is (free-vars '(lambda (foo) (list 23 foo)))
    '(list))
