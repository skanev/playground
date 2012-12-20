; SICP exercise 5.06
;
; Ben Bitdiddle observes that the Fibonacci machine's controller sequence has
; an extra save and an extra restore, which can be removed to make a faster
; machine. Where are these instructions?

; They are below, commented out:

(define fibonacci-machine
  (make-machine
    '(n val continue)
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
;       (restore continue)
        (assign n (op -) (reg n) (const 2))
;       (save continue)
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

