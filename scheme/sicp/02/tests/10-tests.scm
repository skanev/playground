(require rackunit rackunit/text-ui)
(load "../10.scm")

(define sicp-2.10-tests
  (test-suite
    "Tests for SICP exercise 2.10"

    (check-equal? (div-interval (make-interval 10.0 20.0)
                                (make-interval 2.0 5.0))
                  (make-interval 2.0 10.0))

    (check-exn exn? (lambda () (div-interval (make-interval 10 20)
                                             (make-interval -1 1))))
))

(run-tests sicp-2.10-tests)
