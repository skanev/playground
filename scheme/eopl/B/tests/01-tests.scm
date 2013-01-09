(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../01.scm")

(define eopl-B.01-tests
  (test-suite
    "Tests for EOPL exercise B.01"

    (check-equal? (scan&parse "3 + 2 * 66 - 5")
                  (op (op (number 3) '() '())
                      '(+ -)
                      (list
                        (op (number 2) '(*) (list (number 66)))
                        (op (number 5) '() ()))))
))

(exit (run-tests eopl-B.01-tests))
