(require rackunit rackunit/text-ui)
(load "helpers/simulator.scm")
(load "../12.scm")
(load "helpers/sample-machines.scm")

(define sicp-5.12-tests
  (test-suite
    "Tests for SICP exercise 5.12"

    (check-equal? (fibonacci-machine 'data-path-instructions)
                  '((assign continue (label after-fib-n-1))
                    (assign continue (label after-fib-n-2))
                    (assign continue (label fib-done))
                    (assign n (op -) (reg n) (const 1))
                    (assign n (op -) (reg n) (const 2))
                    (assign n (reg val))
                    (assign val (op +) (reg val) (reg n))
                    (assign val (reg n))
                    (branch (label immediate-answer))
                    (goto (label fib-loop))
                    (goto (reg continue))
                    (restore continue)
                    (restore n)
                    (restore val)
                    (save continue)
                    (save n)
                    (save val)
                    (test (op <) (reg n) (const 2))))

    (check-equal? (fibonacci-machine 'data-path-entry-point-registers)
                  '(continue))

    (check-equal? (fibonacci-machine 'data-path-stack-registers)
                  '(continue n val))

    (check-equal? (fibonacci-machine 'data-path-register-sources)
                  '((continue (label after-fib-n-1)
                              (label after-fib-n-2)
                              (label fib-done))
                    (n ((op -) (reg n) (const 1))
                       ((op -) (reg n) (const 2))
                       (reg val))
                    (val ((op +) (reg val) (reg n))
                         (reg n))))

    (check-equal? (factorial-machine 'data-path-register-sources)
                  '((continue (label after-fact)
                              (label fact-done))
                    (n ((op -) (reg n) (const 1)))
                    (val ((op *) (reg n) (reg val))
                         (const 1))))
))

(run-tests sicp-5.12-tests)
