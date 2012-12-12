(require rackunit rackunit/text-ui)
(load "helpers/query.scm")
(load "../58.scm")

(define sicp-4.58-tests
  (test-suite
    "Tests for SICP exercise 4.58"

    (check-equal? (matches-of '(big-shot ?person))
                  '((big-shot (Warbucks Oliver))
                    (big-shot (Bitdiddle Ben))
                    (big-shot (Scrooge Eben))))
))

(run-tests sicp-4.58-tests)
