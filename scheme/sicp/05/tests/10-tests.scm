(require rackunit rackunit/text-ui)
(load "helpers/simulator.scm")
(load "../10.scm")

(define sicp-5.10-tests
  (test-suite
    "Tests for SICP exercise 5.10"

    (test-case "(fibonacci 8)"
      (set-register-contents! fibonacci-machine 'n 8)
      (start fibonacci-machine)

      (check-eq? (get-register-contents fibonacci-machine 'val)
                 21))
))

(run-tests sicp-5.10-tests)
