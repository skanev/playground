(require rackunit rackunit/text-ui)
(load "../73.scm")

(define sicp-3.73-tests
  (test-suite
    "Tests for SICP exercise 3.73"

    (check-equal? (stream-take (RC1 ones-and-zeroes 0) 10)
                  '(5 0.5 5.5 1.0 6.0 1.5 6.5 2.0 7.0 2.5))
))

(run-tests sicp-3.73-tests)
