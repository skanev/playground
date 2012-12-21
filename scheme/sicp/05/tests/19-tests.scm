(require rackunit rackunit/text-ui)
(load "helpers/simulator.scm")
(load "../19.scm")

(define (test-machine)
  (make-machine '(a)
                '()
                '(start
                    (assign a (const 1))
                    (assign a (const 2))
                    (assign a (const 3))
                    (assign a (const 4))
                    (assign a (const 5))
                  before-six
                    (assign a (const 6))
                    (assign a (const 7))
                    (assign a (const 8))
                    (assign a (const 9))
                    (assign a (const 10)))))


(define sicp-5.19-tests
  (test-suite
    "Tests for SICP exercise 5.19"

    (test-case "Checking breakpoints"
      (define machine (test-machine))

      (set-breakpoint machine 'start 3)
      (set-breakpoint machine 'start 5)
      (set-breakpoint machine 'before-six 3)

      (start machine)
      (check-eq? (get-register-contents machine 'a) 2)

      (proceed-machine machine)
      (check-eq? (get-register-contents machine 'a) 4)

      (proceed-machine machine)
      (check-eq? (get-register-contents machine 'a) 7))

    (test-case "Canceling breakpoints"
      (define machine (test-machine))

      (set-breakpoint machine 'start 3)
      (set-breakpoint machine 'start 5)
      (set-breakpoint machine 'before-six 3)

      (cancel-breakpoint machine 'start 3)

      (start machine)
      (check-eq? (get-register-contents machine 'a) 4)

      (cancel-all-breakpoints machine)
      (proceed-machine machine)
      (check-eq? (get-register-contents machine 'a) 10))
))

(run-tests sicp-5.19-tests)
