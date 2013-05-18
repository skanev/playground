(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../27.scm")
(load-relative "helpers/proc.scm")

(define (result-of code)
  (parameterize ((current-output-port (open-output-string)))
    (run code)))

(define (output-of code)
  (with-output-to-string
    (lambda () (run code))))

(define eopl-3.27-tests
  (test-suite
    "Tests for EOPL exercise 3.27"

    (check-equal? (output-of "let f = traceproc(x) x
                               in let g = traceproc(y) (f -(y, 1))
                                  in (g 7)")
                  (string-join '("enter: y = (num-val 7)"
                                 "enter: x = (num-val 6)"
                                 "exit: x"
                                 "exit: y"
                                 "")
                               "\n"))

    (check-equal? (result-of "let f = traceproc (x) -(x, 11)
                              in (f (f 77))")
                  55)

    (check-equal? (result-of "(traceproc (f) (f (f 77))
                               traceproc (x) -(x, 11))")
                  55)

    (check-equal? (result-of "let x = 200
                              in let f = traceproc (z) -(z, x)
                                 in let x = 100
                                    in let g = traceproc (z) -(z, x)
                                       in -((f 1), (g 1))")
                  -100)
 ))

(exit (run-tests eopl-3.27-tests))
