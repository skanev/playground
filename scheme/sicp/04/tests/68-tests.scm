(require rackunit rackunit/text-ui)
(load-relative "helpers/query.scm")
(load "../68.scm")

(define sicp-4.68-tests
  (test-suite
    "Tests for SICP exercise 4.68"

    (check-equal? (matches-of '(reverse (1 2 3) ?x))
                  '((reverse (1 2 3) (3 2 1))))

))

(run-tests sicp-4.68-tests)
