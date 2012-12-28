(require rackunit rackunit/text-ui)
(load "../41.scm")

(define sicp-5.41-tests
  (test-suite
    "Tests for SICP exercise 5.41"

    (check-equal? (find-variable 'z '((y z) (a b c d e) (x y)))
                  '(0 1))

    (check-equal? (find-variable 'c '((y z) (a b c d e) (x y)))
                  '(1 2))

    (check-equal? (find-variable 'x '((y z) (a b c d e) (x y)))
                  '(2 0))

    (check-equal? (find-variable 'w '((y z) (a b c d e) (x y)))
                  'not-found)

    (check-equal? (find-variable 'x '())
                  'not-found)
))

(run-tests sicp-5.41-tests)
