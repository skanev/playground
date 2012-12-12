(require rackunit rackunit/text-ui)
(load "helpers/query.scm")
(load "../61.scm")

(define sicp-4.61-tests
  (test-suite
    "Tests for SICP exercise 4.61"

    (check-equal? (matches-of query-1) response-1)
    (check-equal? (matches-of query-2) response-2)
))

(run-tests sicp-4.61-tests)
