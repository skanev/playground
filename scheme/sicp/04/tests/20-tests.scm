(require rackunit rackunit/text-ui)
(load "../20.scm")

(define (run exp)
  (evaluate exp (setup-environment)))

(define sicp-4.20-tests
  (test-suite
    "Tests for SICP exercise 4.20"

    (check-equal? (letrec->combination '(letrec ((a 1)
                                                 (b 2))
                                          (+ a b)))
                  '(let ((a '*unassigned*)
                         (b '*unassigned*))
                     (set! a 1)
                     (set! b 2)
                     (+ a b)))
    (check-equal? '(#t #f)
                  (run '(begin (define (f x)
                                 (letrec ((even?
                                            (lambda (n)
                                              (if (= n 0)
                                                true
                                                (odd? (- n 1)))))
                                          (odd?
                                            (lambda (n)
                                              (if (= n 0)
                                                false
                                                (even? (- n 1))))))
                                   (list (odd? x) (even? x))))
                               (f 3))))
    (check-equal? 3628800
                  (run '(letrec ((fact
                                   (lambda (n)
                                     (if (= n 1)
                                       1
                                       (* n (fact (- n 1)))))))
                          (fact 10))))
))

(run-tests sicp-4.20-tests)
