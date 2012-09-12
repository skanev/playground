(require rackunit rackunit/text-ui)
(load "../80.scm")

(define sicp-3.80-tests
  (test-suite
    "Tests for SICP exercise 3.80"

    (check-equal? (stream-take (RLC1 10 0) 4)
                  '((10 . 0)
                    (10 . 1.0)
                    (9.5 . 1.9)
                    (8.55 . 2.66)))
))

(run-tests sicp-3.80-tests)
