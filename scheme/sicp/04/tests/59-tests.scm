(require rackunit rackunit/text-ui)
(load "helpers/query.scm")
(load "../59.scm")

(define sicp-4.59-tests
  (test-suite
    "Tests for SICP exercise 4.59"

    (check-equal? (matches-of bens-query)
                  '((meeting administration (Friday 1pm))))

    (check-equal? (matches-of alyssas-query)
                  '((meeting-time (Hacker Alyssa P) (Wednesday 3pm))
                    (meeting-time (Hacker Alyssa P) (Wednesday 4pm))))
))

(run-tests sicp-4.59-tests)
