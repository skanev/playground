(require rackunit rackunit/text-ui)
(load "helpers/simulator.scm")
(load "../04.scm")

(define sicp-5.04-tests
  (test-suite
    "Tests for SICP exercise 5.04"

    (test-case "(recursive-expt 2 8)"
      (set-register-contents! recursive-expt-machine 'b 2)
      (set-register-contents! recursive-expt-machine 'n 8)
      (start recursive-expt-machine)

      (check-eq? (get-register-contents recursive-expt-machine 'val)
                256))

    (test-case "(iterative-expt 2 8)"
      (set-register-contents! iterative-expt-machine 'b 2)
      (set-register-contents! iterative-expt-machine 'n 8)
      (start iterative-expt-machine)

      (check-eq? (get-register-contents iterative-expt-machine 'val)
                256))
))

(run-tests sicp-5.04-tests)
