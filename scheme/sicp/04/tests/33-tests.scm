(require rackunit rackunit/text-ui)
(load "../33.scm")

(define (run exp)
  (actual-value exp (setup-environment)))

(define sicp-4.33-tests
  (test-suite
    "Tests for SICP exercise 4.33"

    (check-true (run '(null? '())))
    (check-true (run '(null? (cdr '(a)))))
    (check-true (run '(null? (car '(() a)))))
    (check-equal? 'a (run '(car '(a b c))))
    (check-equal? 'c (run '(car (cdr (car (cdr '(a (b c))))))))
    (check-equal? '() (run '(car '(() a))))
    (check-equal? '() (run '(cdr (cdr '(() a)))))
))

(run-tests sicp-4.33-tests)
