(require rackunit rackunit/text-ui)
(load "helpers/evaluator.scm")
(load "../24.scm")

(define (run exp)
  (run-controller ec-cond extra-operations exp))

(define sicp-5.24-tests
  (test-suite
    "Tests for SICP exercise 5.24"

    (test-suite "Cond"
      (check-equal? (run '(cond (false 1))) #f)
      (check-equal? (run '(cond (true 1))) 1)
      (check-equal? (run '(cond (false 1) (true 2))) 2)
      (check-equal? (run '(cond (false 1) (else 2))) 2)

      (check-equal? (run '(begin (define a 1)
                                 (cond (true a))))
                    1)
      (check-equal? (run '(begin (define a 1)
                                 (define b 2)
                                 (cond (false a) (true b))))
                    2)
      (check-equal? (run '(begin (define a 1)
                                 (define b 2)
                                 (cond (false a) (else b))))
                    2))
))

(run-tests sicp-5.24-tests)
