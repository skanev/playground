(require rackunit rackunit/text-ui)
(load "../16.scm")

(define sicp-3.16-tests
  (test-suite
    "Tests for SICP exercise 3.16"

    (check-equal? (count-pairs three) 3)
    (check-equal? (count-pairs four) 4)
    (check-equal? (count-pairs seven) 7)
))

(run-tests sicp-3.16-tests)
