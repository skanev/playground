(require rackunit rackunit/text-ui)
(load "helpers/simulator.scm")
(load "../02.scm")

(define sicp-5.02-tests
  (test-suite
    "Tests for SICP exercise 5.02"

    (test-case "(factorial 5)"
      (set-register-contents! factorial-machine 'n 5)
      (start factorial-machine)

      (check-eq? (get-register-contents factorial-machine 'p)
                 120))
))

(run-tests sicp-5.02-tests)
