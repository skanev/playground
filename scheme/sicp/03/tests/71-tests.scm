(require rackunit rackunit/text-ui)
(load "../71.scm")

(define sicp-3.71-tests
  (test-suite
    "Tests for SICP exercise 3.71"

    (check-equal? (stream-take (ramanujan-numbers) 6)
                  '(1729 4104 13832 20683 32832 39312))
))

(run-tests sicp-3.71-tests)
