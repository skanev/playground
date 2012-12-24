(require rackunit rackunit/text-ui)
(load "helpers/evaluator.scm")
(load "../23.scm")

(define (run exp)
  (run-controller ec-cond-and-let extra-operations exp))

(define sicp-5.23-tests
  (test-suite
    "Tests for SICP exercise 5.23"

    (test-suite "Cond"
      (check-equal? (run '(cond (true 1))) 1)
      (check-equal? (run '(cond (false 1) (true 2))) 2)
      (check-equal? (run '(cond (false 1) (else 2))) 2)
      (check-exn exn? (lambda () (run '(cond (else 1) (true 2))))))

    (test-suite "Let"
      (check-equal? (run '(let ((x 1)) x)) 1)
      (check-equal? (run '(let ((a 1) (b 2)) (+ a b))) 3)
      (check-equal? (run '(let ((a 1) (b 2)) a b)) 2)
      (check-equal? (run '(begin (define a 1)
                                 (let ((b 2)
                                       (c 3))
                                   (+ a b c))))
                    6))
))

(run-tests sicp-5.23-tests)
