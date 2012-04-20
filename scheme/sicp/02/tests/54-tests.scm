(require rackunit rackunit/text-ui)
(load "../54.scm")

(define sicp-2.54-tests
  (test-suite
    "Tests for SICP exercise 2.54"

    (check-true (equal2? 'a 'a))
    (check-true (equal2? '(a b) '(a b)))
    (check-true (equal2? '(a (b c) d) '(a (b c) d)))
    (check-true (equal2? '(a (b c) d (e f)) '(a (b c) d (e f))))
))

(run-tests sicp-2.54-tests)
