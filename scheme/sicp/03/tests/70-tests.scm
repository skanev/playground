(require rackunit rackunit/text-ui)
(load "../70.scm")

(define sicp-3.70-tests
  (test-suite
    "Tests for SICP exercise 3.70"

    (check-equal? (stream-take (a-pairs) 20)
                  '((1 1) (1 2) (2 2) (1 3) (2 3) (1 4) (3 3)
                    (2 4) (1 5) (3 4) (2 5) (1 6) (4 4) (3 5)
                    (2 6) (1 7) (4 5) (3 6) (2 7) (1 8)))
    (check-equal? (stream-take (b-pairs) 20)
                  '((1 1) (1 7) (1 11) (1 13) (1 17) (1 19) (1 23)
                    (1 29) (1 31) (7 7) (1 37) (1 41) (1 43) (1 47)
                    (1 49) (1 53) (7 11) (1 59) (1 61) (7 13)))
))

(run-tests sicp-3.70-tests)
