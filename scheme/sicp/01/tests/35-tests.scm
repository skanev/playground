(require rackunit rackunit/text-ui)
(load "../35.scm")

(define sicp-1.35-tests
  (test-suite
    "Tests for SICP exercise 1.35"

    (check-= (* golden-ratio golden-ratio) (+ 1 golden-ratio) 0.00001)
))

(run-tests sicp-1.35-tests)
