(require rackunit rackunit/text-ui)
(load "../35.scm")

(define sicp-2.35-tests
  (test-suite
    "Tests for SICP exercise 2.35"

    (check-equal? (count-leaves '()) 0)
    (check-equal? (count-leaves '(1 2 3 4)) 4)
    (check-equal? (count-leaves '(1 2 (3 4 (5 6) 7) 8 (9))) 9)
))

(run-tests sicp-2.35-tests)
