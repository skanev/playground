(require rackunit rackunit/text-ui)
(require eopl)
(load-relative "../34.scm")

(define eopl-1.34-tests
  (test-suite
    "Tests for EOPL exercise 1.34"

    (check-equal? (path 17 '(14 (7 () (12 () ()))
                                (26 (20 (17 () ())
                                        ())
                                    (31 () ()))))
                  '(right left left))
))

(exit (run-tests eopl-1.34-tests))
