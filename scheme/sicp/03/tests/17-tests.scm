(require rackunit rackunit/text-ui)
(load "../17.scm")

(define a '(a))
(define b (cons 'b a))
(define c (cons a a))

(define three '(a b c))
(define four (cons b a))
(define seven (cons c c))

(define sicp-3.17-tests
  (test-suite
    "Tests for SICP exercise 3.17"

    (check-equal? (count-pairs three) 3)
    (check-equal? (count-pairs four) 3)
    (check-equal? (count-pairs seven) 3)
))

(run-tests sicp-3.17-tests)
