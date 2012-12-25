(require rackunit rackunit/text-ui)
(load "helpers/evaluator.scm")
(load "../30.scm")

(define (run exp)
  (run-controller ec-error-support extra-operations exp))

(define sicp-5.30-tests
  (test-suite
    "Tests for SICP exercise 5.30"

    (check-equal? (run 'x) 'unbound-variable)
    (check-equal? (run '(+ 1 x)) 'unbound-variable)

    (check-equal? (run '(car '(a))) 'a)
    (check-equal? (run '(car '())) 'car-on-null)
    (check-equal? (run '(car 1)) 'car-on-non-pair)
    (check-equal? (run '(+ 1 (car '()))) 'car-on-null)
    (check-equal? (run '(+ 1 (car 1))) 'car-on-non-pair)

    (check-equal? (run '(/ 10 5)) 2)
    (check-equal? (run '(/ 10 0)) 'zero-division-error)
    (check-equal? (run '(+ 1 (/ 10 0))) 'zero-division-error)

    (check-equal? (run '(+ 2 3)) 5)
))

(run-tests sicp-5.30-tests)
