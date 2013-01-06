(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../13.scm")

(define eopl-2.13-tests
  (test-suite
    "Tests for EOPL exercise 2.13"

    (check-exn exn? (lambda () (apply-env (empty-env) 'a)))
    (check-exn exn? (lambda () (apply-env (extend-env 'a 1 (empty-env)) 'b)))
    (check-equal? (apply-env (extend-env 'a 1 (empty-env)) 'a)
                  1)
    (check-equal? (apply-env (extend-env 'a 1 (extend-env 'b 2 (empty-env))) 'b)
                  2)
    (check-equal? (apply-env (extend-env 'a 3
                               (extend-env 'b 2
                                 (extend-env 'a 1
                                   (empty-env))))
                             'a)
                  3)

    (check-false (empty-env? (extend-env 'a 1 (empty-env))))
    (check-true (empty-env? (empty-env)))
))

(exit (run-tests eopl-2.13-tests))
