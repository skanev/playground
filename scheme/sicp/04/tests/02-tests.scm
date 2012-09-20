(require rackunit rackunit/text-ui)
(load "../02.scm")

(define (run exp)
  (evaluate exp (setup-environment)))

(define sicp-4.02-tests
  (test-suite
    "Tests for SICP exercise 4.02"

    (check-equal? (run '(begin (define (factorial n)
                                 (if (call = n 1)
                                     1
                                     (call * n (call factorial (call - n 1)))))
                               (call factorial 5)))
                  120)
    (check-equal? (run '(begin (define (factorial n)
                                 (define (iter n result)
                                   (if (call = n 0)
                                       result
                                       (call iter (call - n 1) (call * result n))))
                                 (call iter n 1))
                               (call factorial 5)))
                  120)
))

(run-tests sicp-4.02-tests)
