(require rackunit rackunit/text-ui)
(load "../85.scm")

(define sicp-2.85-tests
  (test-suite
    "Tests for SICP exercise 2.85"

    (check-true (equ? (make-integer 1) (make-integer 1)))
    (check-true (equ? (make-rational 1 2) (make-rational 2 4)))
    (check-true (equ? (make-real 1.5) (make-real 1.5)))
    (check-true (equ? (make-complex 1 2) (make-complex 1 2)))

    (check-false (equ? (make-integer 1) (make-integer 2)))
    (check-false (equ? (make-rational 1 2) (make-rational 1 3)))
    (check-false (equ? (make-rational 1 2) (make-rational 2 2)))
    (check-false (equ? (make-real 1.5) (make-real 2.5)))
    (check-false (equ? (make-complex 1 2) (make-complex 1 3)))
    (check-false (equ? (make-complex 1 2) (make-complex 2 2)))

    (check-equal? (project (make-complex 1.0 2.0)) (make-real 1.0))
    (check-equal? (project (make-real 2.5)) (make-rational 25 10))
    (check-equal? (project (make-rational 5 2)) (make-integer 2))

    (check-equal? (drop (make-complex 1 2)) (make-complex 1 2))
    (check-equal? (drop (make-complex 2.5 0)) (make-rational 5 2))
    (check-equal? (drop (make-complex 1 0)) (make-integer 1))
    (check-equal? (drop (make-real 2.5)) (make-rational 5 2))
    (check-equal? (drop (make-real 1.0)) (make-integer 1))
    (check-equal? (drop (make-rational 1 2)) (make-rational 1 2))
    (check-equal? (drop (make-rational 1 1)) (make-integer 1))
    (check-equal? (drop (make-integer 1)) (make-integer 1))

    (check-equal? (add (make-complex 1 2) (make-complex 3 4)) (make-complex 4 6))
    (check-equal? (add (make-complex 1 1) (make-complex 2 -1)) (make-integer 3))
    (check-equal? (add (make-real 1.5) (make-real 2.5)) (make-integer 4))
))

(run-tests sicp-2.85-tests)
