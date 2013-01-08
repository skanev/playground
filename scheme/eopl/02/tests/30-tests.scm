(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../30.scm")

(define eopl-2.30-tests
  (test-suite
    "Tests for EOPL exercise 2.30"

    (check-equal? (parse 'a)
                  (var-exp 'a))
    (check-equal? (parse '(lambda (a) a))
                  (lambda-exp 'a (var-exp 'a)))
    (check-equal? (parse '(a b))
                  (app-exp (var-exp 'a)
                           (var-exp 'b)))

    (check-exn (regexp "lambda is not a valid identifier")
               (lambda () (parse 'lambda)))
    (check-exn (regexp "lambda requires two components. given: \\(lambda\\)")
               (lambda () (parse '(lambda))))
    (check-exn (regexp "lambda requires two components. given: \\(lambda a\\)")
               (lambda () (parse '(lambda a))))
    (check-exn (regexp "lambda requires two components. given: \\(lambda a b c\\)")
               (lambda () (parse '(lambda a b c))))
    (check-exn (regexp "lambda requires an arglist. given: a")
               (lambda () (parse '(lambda a b))))
    (check-exn (regexp "lambda requires exactly one argument. given: \\(\\)")
               (lambda () (parse '(lambda () a))))
    (check-exn (regexp "lambda requires exactly one argument. given: \\(a b\\)")
               (lambda () (parse '(lambda (a b) a))))

    (check-exn (regexp "application requires two components. given: \\(a\\)")
               (lambda () (parse '(a))))
    (check-exn (regexp "application requires two components. given: \\(a b c\\)")
               (lambda () (parse '(a b c))))
))

(exit (run-tests eopl-2.30-tests))
