(require rackunit rackunit/text-ui)
(load "helpers/query.scm")
(load "../62.scm")

(define sicp-4.62-tests
  (test-suite
    "Tests for SICP exercise 4.62"

    (check-equal? (matches-of '(last-pair (3) ?x))
                  '((last-pair (3) (3))))

    (check-equal? (matches-of '(last-pair (1 2 3) ?x))
                  '((last-pair (1 2 3) (3))))

    (check-equal? (matches-of '(last-pair (2 ?x) (3)))
                  '((last-pair (2 3) (3))))
))

(run-tests sicp-4.62-tests)
