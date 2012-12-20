(require rackunit rackunit/text-ui)
(load "helpers/simulator.scm")
(load "../09.scm")

(define sicp-5.09-tests
  (test-suite
    "Tests for SICP exercise 5.09"

    (check-exn (regexp "Operations are not applicable to labels")
               (lambda ()
                 (make-machine
                   '(a)
                   (list (list '+ +))
                   '(foo
                      (assign a (op +) (label foo) (label bar))
                     bar))))


))

(run-tests sicp-5.09-tests)
