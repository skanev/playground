(require rackunit rackunit/text-ui)

(load "simulator.scm")
(load "sample-machines.scm")

(define simulator-tests
  (test-suite "Register machine simulator tests"

    (test-case "(gcd 206 40)"
      (set-register-contents! gcd-machine 'a 206)
      (set-register-contents! gcd-machine 'b 40)
      (start gcd-machine)

      (check-eq? (get-register-contents gcd-machine 'a)
                 2))

    (test-case "(gcd 54 24)"
      (set-register-contents! gcd-machine 'a 54)
      (set-register-contents! gcd-machine 'b 24)
      (start gcd-machine)

      (check-eq? (get-register-contents gcd-machine 'a)
                 6))

    (test-case "(gcd 13 7)"
      (set-register-contents! gcd-machine 'a 13)
      (set-register-contents! gcd-machine 'b 7)
      (start gcd-machine)

      (check-eq? (get-register-contents gcd-machine 'a)
                 1))

    (test-case "(factorial 5)"
      (set-register-contents! factorial-machine 'n 5)
      (start factorial-machine)

      (check-eq? (get-register-contents factorial-machine 'val)
                 120))

    (test-case "(fibonacci 8)"
      (set-register-contents! fibonacci-machine 'n 8)
      (start fibonacci-machine)

      (check-eq? (get-register-contents fibonacci-machine 'val)
                 21))
))

(run-tests simulator-tests)
