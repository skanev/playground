(require rackunit rackunit/text-ui)
(load "../83.scm")

(define sicp-2.83-tests
  (test-suite
    "Tests for SICP exercise 2.83"

    (check-equal? (raise (make-integer 4)) (make-rational 4 1))
    (check-equal? (raise (make-rational 5 2)) (make-real 2.5))
    (check-equal? (raise (make-real 2.0)) (make-complex 2.0 0))

))

(run-tests sicp-2.83-tests)
