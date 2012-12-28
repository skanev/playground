; SICP exercise 5.47
;
; This section described how to modify the explicit-control evaluator so that
; interpreted code can call compiled procedures. Show how to modify the
; compiler so that compiled procedures can call not only primitive procedures
; and compiled procedures, but interpreted procedures as well. This requires
; modifying compile-procedure-call to handle the case of compound
; (interpreted) procedures. Be sure to handle all the same target and linkage
; combinations as in compile-proc-argl. To do the actual procedure
; application, the code needs to jump to the evaluator's compound-apply entry
; point. This label cannot be directly referenced in object code (since the
; assmebler requires that all labels referenced by the code it is assembling
; be defined there), so we will add a register called compapp to the evaluator
; machine to hold this entry point, and add an instruction to initialize it:
;
;   (assign compapp (label compound-apply))
;   (branch (label external-entry))
; read-eval-print-loop
;   ...
;
; To test your code, start by defining a proceudre f that calls a procedure g.
; Use compile-and-go to compile the definition of f and start the evaluator.
; Now, typing at the evalutor, define g and try to call f.

; Yeah, typing in the evalutor. Right.

(define ec-registers (cons 'compapp ec-registers))

(define explicit+compile-text
  (append '((assign compapp (label compound-apply)))
          explicit+compile-text))

(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (compound-branch (make-label 'compound-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence '(proc) '()
        `((test (op primitive-procedure?) (reg proc))
          (branch (label ,primitive-branch))
          (test (op compound-procedure?) (reg proc))
          (branch (label ,compound-branch))))
       (parallel-instruction-sequences
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl target compiled-linkage))
        (parallel-instruction-sequences
          (append-instruction-sequences
           compound-branch
           (compile-compound-appl target compiled-linkage))
          (append-instruction-sequences
           primitive-branch
           (end-with-linkage linkage
            (make-instruction-sequence '(proc argl) (list target)
             `((assign ,target
                       (op apply-primitive-procedure)
                       (reg proc)
                       (reg argl))))))))
       after-call))))

(define (compile-compound-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc) all-regs
          `((assign continue (label ,linkage))
            (save continue)
            (goto (reg compapp)))))
        ((and (not (eq? target 'val)) (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence '(proc) all-regs
            `((assign continue (label ,proc-return))
              (save continue)
              (goto (reg compapp))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence '(proc continue) all-regs
          `((save continue)
            (goto (reg compapp)))))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE-EXP" target))
        (else (error "How did we get here?"))))
