(require rackunit rackunit/text-ui)
(load "helpers/simulator.scm")
(load "../13.scm")

(define fibonacci-machine
  (make-machine
    (list (list '< <) (list '- -) (list '+ +))
    '(
        (assign continue (label fib-done))
      fib-loop
        (test (op <) (reg n) (const 2))
        (branch (label immediate-answer))
        (save continue)
        (assign continue (label after-fib-n-1))
        (save n)
        (assign n (op -) (reg n) (const 1))
        (goto (label fib-loop))
      after-fib-n-1
        (restore n)
        (restore continue)
        (assign n (op -) (reg n) (const 2))
        (save continue)
        (assign continue (label after-fib-n-2))
        (save val)
        (goto (label fib-loop))
      after-fib-n-2
        (assign n (reg val))
        (restore val)
        (restore continue)
        (assign val (op +) (reg val) (reg n))
        (goto (reg continue))
      immediate-answer
        (assign val (reg n))
        (goto (reg continue))
      fib-done)))

(define sicp-5.13-tests
  (test-suite
    "Tests for SICP exercise 5.13"

    (test-case "(fibonacci 8)"
      (set-register-contents! fibonacci-machine 'n 8)
      (start fibonacci-machine)

      (check-eq? (get-register-contents fibonacci-machine 'val)
                 21))
))

(run-tests sicp-5.13-tests)
