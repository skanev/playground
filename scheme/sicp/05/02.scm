; SICP exercise 5.02
;
; Use the register machine language to describe the iterative factorial
; machine of exercise 5.1.

(define factorial-machine
  (make-machine
    '(c p n)
    (list (list '+ +) (list '* *) (list '> >))
    '(
        (assign c (const 1))
        (assign p (const 1))
      test->
        (test (op >) (reg c) (reg n))
        (branch (label factorial-done))
        (assign p (op *) (reg c) (reg p))
        (assign c (op +) (reg c) (const 1))
        (goto (label test->))
      factorial-done)))
