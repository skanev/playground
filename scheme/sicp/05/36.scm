; SICP exercise 5.36
;
; What order of evaluation does our compiler produce for operands of a
; combination? Is it left-to-right, right-to-left, or some other order? Where
; in the compiler is this order determined? Modify the compiler so that it
; produces some other order of evaluation. (See the discussion of order of
; evaluation for the explicit-control evaluator in section 5.4.1). How does
; changing the order of operand evaluation affect the efficiency of the code
; that constructs the argument list?

; The compiler evaluates right-to-left. We can modify it by not reversing in
; construct-arglist and reverse argl instead, once we've accumulated all the
; arguments in it. This is less efficient, since we're doing the reverse
; run-time instead of compile-time.

(define extra-operations
  (list (list 'reverse reverse)))

(define (construct-arglist operand-codes)
  (let ((operand-codes operand-codes))
    (if (null? operand-codes)
        (make-instruction-sequence '() '(argl)
                                   '((assign argl (const ()))))
        (let ((code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence '(val) '(argl)
                 '((assign argl (op list) (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving '(env)
                          code-to-get-last-arg
                          (append-instruction-sequences
                            (code-to-get-rest-args (cdr operand-codes))
                            (make-instruction-sequence '(argl) '()
                             '((assign argl (op reverse) (reg argl)))))))))))
