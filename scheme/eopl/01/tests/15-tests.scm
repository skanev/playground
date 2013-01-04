(require rackunit rackunit/text-ui)
(require eopl)
(load-relative "../15.scm")

(define eopl-1.15-tests
  (test-suite
    "Tests for EOPL exercise 1.15"

    (check-equal? (duple 2 3)
                  '(3 3))
    (check-equal? (duple 4 '(ha ha))
                  '((ha ha) (ha ha) (ha ha) (ha ha)))
    (check-equal? (duple 0 '(blah))
                  '())
))

(exit (run-tests eopl-1.15-tests))
