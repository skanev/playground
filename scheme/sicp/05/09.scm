; SICP exercise 5.09
;
; The treatment of machine operations above permits them to operate on labels
; as well as on constants and the contents of registers. Modify the
; expression-processing procedures to enforce the condition that operations
; can be used only with registers and constants.

(define (make-operation-exp exp machine labels operations)
  (if (ormap label-exp? (operation-exp-operands exp))
      (error "Operations are not applicable to labels" exp)
      (let ((op (lookup-prim (operation-exp-op exp) operations))
            (aprocs (map (lambda (e) (make-primitive-exp e machine labels))
                         (operation-exp-operands exp))))
        (lambda ()
          (apply op (map (lambda (p) (p)) aprocs))))))
