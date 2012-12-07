(require rackunit rackunit/text-ui)
(load "../13.scm")

(define (run exp)
  (evaluate exp (setup-environment)))

(define sicp-4.13-tests
  (test-suite
    "Tests for SICP exercise 4.13"

    (check-exn (regexp "Unbound variable x")
               (lambda () (run '(begin (define x 10)
                                       (make-unbound! x)
                                       x))))

    (check-exn (regexp "Cannot unbind a variable that is not declared in the current frame x")
               (lambda () (run '(begin (define x 10)
                                       ((lambda ()
                                          (make-unbound! x)))))))
))

(run-tests sicp-4.13-tests)
