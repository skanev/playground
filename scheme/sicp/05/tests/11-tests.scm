(require rackunit rackunit/text-ui)
(load "helpers/simulator.scm")
(load "../11.scm")

(define sicp-5.11-tests
  (test-suite
    "Tests for SICP exercise 5.11"

    (test-case "a. Shorter Fibonacci machine"
      (use-version-a!)
      (let ((fibonacci-machine (make-shorter-fibonacci-machine)))
        (set-register-contents! fibonacci-machine 'n 8)
        (start fibonacci-machine)

        (check-eq? (get-register-contents fibonacci-machine 'val)
                   21)))

    (test-case "b. Erroring out on restoring wrong register"
      (use-version-b!)
      (check-exn (regexp "Mismatching registers: x \\(restore y\\)")
                 (lambda ()
                   (start (make-machine '(x y)
                                        '()
                                        '((assign x (const 1))
                                          (save x)
                                          (restore y)))))))

    (test-case "c. A stack per register"
      (use-version-c!)
      (let ((machine (make-machine '(x y)
                                   '()
                                   '((assign x (const 1))
                                     (assign y (const 2))
                                     (save x)
                                     (save y)
                                     (assign x (const 3))
                                     (assign y (const 4))
                                     (restore x)
                                     (restore y)))))
        (start machine)

        (check-eq? (get-register-contents machine 'x) 1)
        (check-eq? (get-register-contents machine 'y) 2)))
))

(run-tests sicp-5.11-tests)
