(require rackunit rackunit/text-ui)
(load "../22.scm")

(define (run exp)
  (evaluate exp (setup-environment)))

(define sicp-4.22-tests
  (test-suite
    "Tests for SICP exercise 4.22"

    (check-equal? (run '(let ((x 1)) x)) 1)
    (check-equal? (run '(let ((a 1) (b 2)) (+ a b))) 3)
    (check-equal? (run '(let ((a 1) (b 2)) a b)) 2)
    (check-equal? (run '(begin (define a 1)
                               (let ((b 2)
                                     (c 3))
                                 (+ a b c))))
                  6)
))

(run-tests sicp-4.22-tests)
