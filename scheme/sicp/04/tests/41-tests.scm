(require rackunit rackunit/text-ui)
(load "../41.scm")

(define sicp-4.41-tests
  (test-suite
    "Tests for SICP exercise 4.41"

    (check-equal? (multiple-dwellings)
                  '(((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))))
))

(run-tests sicp-4.41-tests)
