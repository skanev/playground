(require rackunit rackunit/text-ui)
(load "../19.scm")

(define sicp-2.19-tests
  (test-suite
    "Tests for SICP exercise 2.19"

    (check-equal? (cc 100 '(50 25 10 5 1)) 292)
    (check-equal? (cc 11 '(10 5 1)) 4)
))

(run-tests sicp-2.19-tests)
