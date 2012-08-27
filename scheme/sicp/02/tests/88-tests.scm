(require rackunit rackunit/text-ui)
(load "../88.scm")

(define (poly var . coeffs)
  (define (value coeff)
    (cond ((and (number? coeff) (integer? coeff)) (make-integer coeff))
          ((number? coeff) (make-real coeff))
          (else coeff)))
  (define (to-term-list coeffs)
    (cond ((null? coeffs) '())
          ((=zero? (value (car coeffs))) (to-term-list (cdr coeffs)))
          (else (cons (list (- (length coeffs) 1) (value (car coeffs)))
                      (to-term-list (cdr coeffs))))))

  (make-polynomial var (to-term-list coeffs)))

(define sicp-2.88-tests
  (test-suite
    "Tests for SICP exercise 2.88"

    (test-suite "polynomials"
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
    )

    (test-suite "type tower"
      (check-true (supertype? 'integer 'rational))
      (check-true (supertype? 'integer 'real))
      (check-true (supertype? 'rational 'real))
    )

    (test-suite "integers"
      (check-exn exn? (lambda () (make-integer 1.5)))
      (check-equal? (add (make-integer 1) (make-integer 2)) (make-integer 3))
      (check-equal? (sub (make-integer 3) (make-integer 2)) (make-integer 1))
      (check-equal? (mul (make-integer 2) (make-integer 4)) (make-integer 8))
      (check-equal? (neg (make-integer 1)) (make-integer -1))
      (check-true (equ? (make-integer 1) (make-integer 1)))
      (check-false (equ? (make-integer 1) (make-integer 2)))
      (check-equal? (raise (make-integer 2)) (make-rational 2 1))
      (check-true (=zero? (make-integer 0)))
    )

    (test-suite "rationals"
      (check-exn exn? (lambda () (make-rational 1.5 1)))
      (check-exn exn? (lambda () (make-rational 1 1.5)))
      (check-equal? (add (make-rational 1 2) (make-rational 3 4)) (make-rational 5 4))
      (check-equal? (sub (make-rational 3 4) (make-rational 1 2)) (make-rational 1 4))
      (check-equal? (mul (make-rational 2 3) (make-rational 3 6)) (make-rational 1 3))
      (check-equal? (div (make-rational 5 4) (make-rational 1 2)) (make-rational 5 2))
      (check-equal? (neg (make-rational 1 2)) (make-rational -1 2))
      (check-equal? (raise (make-rational 5 2)) (make-real 2.5))
      (check-true (equ? (make-rational 1 2) (make-rational 2 4)))
      (check-false (equ? (make-rational 1 2) (make-rational 1 3)))
      (check-equal? (project (make-rational 5 2)) (make-integer 2))
      (check-true (=zero? (make-rational 0 1)))
    )

    (test-suite "reals"
      (check-equal? (add (make-real 1.5) (make-real 2.0)) (make-real 3.5))
      (check-equal? (sub (make-real 3.5) (make-real 2.0)) (make-real 1.5))
      (check-equal? (mul (make-real 1.25) (make-real 2.0)) (make-real 2.5))
      (check-equal? (div (make-real 5.0) (make-real 2.0)) (make-real 2.5))
      (check-equal? (neg (make-real 2.5)) (make-real -2.5))
      (check-equal? (sine (make-real 1.0)) (make-real (sin 1.0)))
      (check-equal? (cosine (make-real 1.0)) (make-real (cos 1.0)))
      (check-equal? (square-root (make-real 2.0)) (make-real (sqrt 2.0)))
      (check-equal? (arctangent (make-real 3.0) (make-real 4.0)) (make-real (atan 3.0 4.0)))
      (check-true (equ? (make-real 2.5) (make-real 2.5)))
      (check-false (equ? (make-real 2.0) (make-real 2.5)))
      (check-equal? (project (make-real 2.5)) (make-rational 2 1))
      (check-true (=zero? (make-real 0.0)))
    )

    (test-suite "coercions among numbers"
      (check-equal? (div (make-integer 1) (make-integer 2)) (make-rational 1 2))
      (check-equal? (add (make-integer 1) (make-rational 1 2)) (make-rational 3 2))
      (check-equal? (add (make-integer 1) (make-real 2.5)) (make-real 3.5))
      (check-equal? (sine (make-integer 1)) (make-real (sin 1.0)))
      (check-equal? (sine (make-rational 2 2)) (make-real (sin 1.0)))
      (check-equal? (arctangent (make-integer 3) (make-integer 4)) (make-real (atan 3.0 4.0)))
    )

    (test-suite "simplification"
      (check-equal? (simplify (make-rational 2 1)) (make-integer 2))
      (check-equal? (simplify (make-real 4.0)) (make-integer 4))
      (check-equal? (simplify (make-real 2.5)) (make-real 2.5))
    )
))

(run-tests sicp-2.88-tests)
