(require rackunit rackunit/text-ui)
(load "../91.scm")

(define (poly var . coeffs)
  (define (value coeff)
    (cond ((and (number? coeff) (integer? coeff)) (make-integer coeff))
          ((number? coeff) (make-real coeff))
          (else coeff)))

  (make-polynomial var (map value coeffs)))

(define sicp-2.91-tests
  (test-suite
    "Tests for SICP exercise 2.91"

    (check-equal? (add (poly 'x 1 2 3) (poly 'x 4 5 6))
                  (poly 'x 5 7 9))
    (check-equal? (mul (poly 'x 1 1) (poly 'x 1 -1))
                  (poly 'x 1 0 -1))
    (check-equal? (mul (poly 'x 1 (poly 'y 1 0))
                       (poly 'x 1 (poly 'y 1 0)))
                  (poly 'x 1 (poly 'y 2 0) (poly 'y 1 0 0)))

    (check-equal? (neg (poly 'x 1 -2 3))
                  (poly 'x -1 2 -3))
    (check-equal? (sub (poly 'x 4 4 4)
                       (poly 'x 3 2 1))
                  (poly 'x 1 2 3))
    (check-equal? (sub (poly 'x 3 3 3)
                       (poly 'x 3 2 1))
                  (poly 'x 1 2))

    (check-equal? (div (poly 'x 1 0 0 0 0 -1)
                       (poly 'x 1 0 -1))
                  (list (poly 'x 1 0 1 0)
                        (poly 'x 1 -1)))
))

(run-tests sicp-2.91-tests)
