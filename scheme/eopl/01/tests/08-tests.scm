(require rackunit rackunit/text-ui)
(require eopl)
(load-relative "../08.scm")

(define eopl-1.08-tests
  (test-suite
    "Tests for EOPL exercise 1.08"

    (check-equal? (drop-until 'c '(a b c d e f))
                  '(d e f))
))

(exit (run-tests eopl-1.08-tests))
