(require rackunit rackunit/text-ui)
(load "../73.scm")

(define sicp-2.73-tests
  (test-suite
    "Tests for SICP exercise 2.73"

    (check-equal? (deriv '(+ x 3) 'x)
                  1)

    (check-equal? (deriv '(* x y) 'x)
                  'y)

    (check-equal? (deriv '(* (* x y) (+ x 3)) 'x)
                  '(+ (* x y) (* y (+ x 3))))



    (check-equal? (deriv '(** x 3) 'x)
                  '(* 3 (** x 2)))

    (check-equal? (deriv '(** x 2) 'x)
                  '(* 2 x))

    (check-equal? (deriv '(** x 1) 'x)
                  1)
))

(run-tests sicp-2.73-tests)
