(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../06.scm")

(define eopl-2.06-tests
  (test-suite
    "Tests for EOPL exercise 2.06"

    (test-suite "1. 2-list"
      (use-2-list)
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
                    3))

    (test-suite "2. two lists"
      (use-two-lists)
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
                    3))

    (test-suite "3. var-val list"
      (use-var-val-list)
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
                    3))
))

(exit (run-tests eopl-2.06-tests))
