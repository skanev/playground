(require rackunit rackunit/text-ui)
(load "../78.scm")

(define sicp-2.78-tests
  (test-suite
    "Tests for SICP exercise 2.78"

    (test-suite "scheme-number package"
      (check-equal? (add (make-scheme-number 8) (make-scheme-number 2)) (make-scheme-number 10))
      (check-equal? (sub (make-scheme-number 8) (make-scheme-number 2)) (make-scheme-number 6))
      (check-equal? (mul (make-scheme-number 8) (make-scheme-number 2)) (make-scheme-number 16))
      (check-equal? (div (make-scheme-number 8) (make-scheme-number 2)) (make-scheme-number 4))
    )

    (test-suite "rational package"
      (check-equal? (add (make-rational 3 4) (make-rational 1 2)) (make-rational 5 4))
      (check-equal? (sub (make-rational 3 4) (make-rational 1 2)) (make-rational 1 4))
      (check-equal? (mul (make-rational 3 4) (make-rational 1 2)) (make-rational 3 8))
      (check-equal? (div (make-rational 3 4) (make-rational 1 2)) (make-rational 6 4))
    )

    (test-suite "rational package"
      (check-equal? (real-part (make-from-real-imag 3 4)) 3)
      (check-equal? (imag-part (make-from-real-imag 3 4)) 4)
      (check-equal? (magnitude (make-from-real-imag 3 4)) 5)
      (check-equal? (angle (make-from-real-imag 3 4)) (atan 4 3))

      (check-= (real-part ((get 'make-from-mag-ang 'rectangular) 5 (atan 4 3))) 3 0.0001)
      (check-= (imag-part ((get 'make-from-mag-ang 'rectangular) 5 (atan 4 3))) 4 0.0001)
    )

    (test-suite "polar package"
      (check-= (real-part (make-from-mag-ang 5 (atan 4 3))) 3 0.0001)
      (check-= (imag-part (make-from-mag-ang 5 (atan 4 3))) 4 0.0001)
      (check-equal? (magnitude (make-from-mag-ang 3 4)) 3)
      (check-equal? (angle (make-from-mag-ang 3 4)) 4)

      (check-= (magnitude ((get 'make-from-real-imag 'polar) 3 4)) 5 0.0001)
      (check-= (angle ((get 'make-from-real-imag 'polar) 3 4)) (atan 4 3) 0.0001)
    )

    (test-suite "complex package"
      (check-equal?
        (add (make-complex-from-real-imag 1 2) (make-complex-from-real-imag 3 4))
        (make-complex-from-real-imag 4 6))
      (check-equal?
        (sub (make-complex-from-real-imag 6 7) (make-complex-from-real-imag 1 3))
        (make-complex-from-real-imag 5 4))
      (check-equal?
        (mul (make-complex-from-mag-ang 2 4) (make-complex-from-mag-ang 3 5))
        (make-complex-from-mag-ang 6 9))
      (check-equal?
        (div (make-complex-from-mag-ang 6 5) (make-complex-from-mag-ang 3 4))
        (make-complex-from-mag-ang 2 1))

      (check-equal? (real-part (make-complex-from-real-imag 3 4)) 3)
      (check-equal? (imag-part (make-complex-from-real-imag 3 4)) 4)
      (check-equal? (magnitude (make-complex-from-mag-ang 1 2)) 1)
      (check-equal? (angle (make-complex-from-mag-ang 1 2)) 2)

      (check-=
        (real-part
          (mul (make-complex-from-mag-ang 5 (atan 4 3))
               (make-complex-from-real-imag 1 0)))
        3
        0.0001)

      (check-=
        (imag-part
          (mul (make-complex-from-mag-ang 5 (atan 4 3))
               (make-complex-from-real-imag 1 0)))
        4
        0.0001)
    )
))

(run-tests sicp-2.78-tests)
