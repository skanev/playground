(require rackunit rackunit/text-ui)
(load "helpers/compiler.scm")
(load "../42.scm")

(define (run exp)
  (let ((machine (make-explicit+compile-machine)))
    (compile-in-machine machine exp)
    (get-register-contents machine 'val)))

(define sicp-5.42-tests
  (test-suite
    "Tests for SICP exercise 5.42"

    (check-equal? (run '(begin (define a 42)
                               ((lambda () a))))
                  42)

    (check-equal? (run '((lambda (a b) b) 1 2))
                  2)

    (let ((not-so-simple-expression '(((lambda (x y)
                                         (lambda (a b c d e)
                                           ((lambda (y z) (* x y z))
                                            (* a b x)
                                            (+ c d x))))
                                       3 4)
                                      5 6 7 8 9)))
      (check-equal? (run not-so-simple-expression) (eval not-so-simple-expression)))

    (check-equal? (run '(begin (define x 1)
                               (set! x 2)
                               x))
                  2)

    (check-equal? (run '(begin (define (foo x)
                                 (set! x 2)
                                 x)
                               (foo 1)))
                  2)

    (check-equal? (run '(begin ((lambda (a)
                                  ((lambda (b)
                                    (set! a b))
                                   2)
                                  a)
                                1)))
                  2)
))

(run-tests sicp-5.42-tests)
