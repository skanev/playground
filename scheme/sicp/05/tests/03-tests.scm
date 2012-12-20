(require rackunit rackunit/text-ui)
(load "helpers/simulator.scm")
(load "../03.scm")

(define sicp-5.03-tests
  (test-suite
    "Tests for SICP exercise 5.03"

    (test-case "(simple-sqrt 2.0)"
      (set-register-contents! simple-sqrt-machine 'x 2.0)
      (start simple-sqrt-machine)

      (check-= (get-register-contents simple-sqrt-machine 'g)
               1.41421
               0.00001))

    (test-case "(simple-sqrt 4.0)"
      (set-register-contents! simple-sqrt-machine 'x 4.0)
      (start simple-sqrt-machine)

      (check-= (get-register-contents simple-sqrt-machine 'g)
               2.0
               0.00001))

    (test-case "(complex-sqrt 2.0)"
      (set-register-contents! complex-sqrt-machine 'x 2.0)
      (start complex-sqrt-machine)

      (check-= (get-register-contents complex-sqrt-machine 'g)
               1.41421
               0.00001))

    (test-case "(complex-sqrt 4.0)"
      (set-register-contents! complex-sqrt-machine 'x 4.0)
      (start complex-sqrt-machine)

      (check-= (get-register-contents complex-sqrt-machine 'g)
               2.0
               0.00001))
))

(run-tests sicp-5.03-tests)
