(require rackunit rackunit/text-ui)
(load "../66.scm")

(define sicp-3.66-tests
  (test-suite
    "Tests for SICP exercise 3.66"

    (check-equal? (location int-pairs '(1 100)) (position '(1 100)))
    (check-equal? (location int-pairs '(7 30)) (position '(7 30)))
))

(run-tests sicp-3.66-tests)
