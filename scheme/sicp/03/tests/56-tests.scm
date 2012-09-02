(require rackunit rackunit/text-ui)
(load "../56.scm")

(define sicp-3.56-tests
  (test-suite
    "Tests for SICP exercise 3.56"

    (check-equal? (stream-take S 20)
                  '(1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36))
))

(run-tests sicp-3.56-tests)
