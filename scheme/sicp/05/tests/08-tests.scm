(require rackunit rackunit/text-ui)
(load "helpers/simulator.scm")
(load "../08.scm")

(define sicp-5.08-tests
  (test-suite
    "Tests for SICP exercise 5.08"

    (check-exn (regexp "Duplicate label: here")
               (lambda ()
                 (make-machine
                   '(a)
                   '()
                   '(
                       (goto (label here))
                     here
                       (assign a (const 3))
                       (goto (label there))
                     here
                       (assign a (const 4))
                       (goto (label there))
                     there))))
))

(run-tests sicp-5.08-tests)
