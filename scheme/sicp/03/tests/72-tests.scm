(require rackunit rackunit/text-ui)
(load "../72.scm")

(define sicp-3.72-tests
  (test-suite
    "Tests for SICP exercise 3.72"

    (check-equal? (stream-take (three-ways-of-two-squares) 20)
                  '( 325  425  650  725  845  850  925 1025 1105 1250
                    1300 1325 1445 1450 1525 1625 1690 1700 1825 1850))
))

(run-tests sicp-3.72-tests)
