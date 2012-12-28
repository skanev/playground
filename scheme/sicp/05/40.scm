; SICP exercise 5.40
;
; Modify the compiler to maintain the compile-time environment as described
; above. That is, add a compile-time-environment argument to compile and the
; various code-generators, and extend it in compile-lambda-body

(load-relative "39.scm")

; Construction of the compile-time environment

(define (empty-compile-time-env) '())
(define (extend-compile-time-environment formals env) (cons formals env))

; The modifications of the compiler

(define (compile-exp exp target linkage env)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage env))
        ((assignment? exp)
         (compile-assignment exp target linkage env))
        ((definition? exp)
         (compile-definition exp target linkage env))
        ((if? exp) (compile-if exp target linkage env))
        ((lambda? exp) (compile-lambda exp target linkage env))
        ((begin? exp)
         (compile-sequence (begin-actions exp) target linkage env))
        ((cond? exp) (compile-exp (cond->if exp) target linkage env))
        ((application? exp)
         (compile-application exp target linkage env))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

(define (compile-variable exp target linkage env)
  (end-with-linkage linkage
   (make-instruction-sequence '(env) (list target)
    `((assign ,target (op lookup-variable-value) (const ,exp) (reg env))))))

(define (compile-assignment exp target linkage env)
  (let ((var (assignment-variable exp))
        (get-value-code (compile-exp (assignment-value exp) 'val 'next env)))
    (end-with-linkage linkage
     (preserving '(env)
      get-value-code
      (make-instruction-sequence '(env val) (list target)
       `((perform (op set-variable-value!)
                  (const ,var)
                  (reg val)
                  (reg env))
         (assign ,target (const ok))))))))

(define (compile-definition exp target linkage env)
  (let ((var (definition-variable exp))
        (get-value-code (compile-exp (definition-value exp) 'val 'next env)))
    (end-with-linkage linkage
     (preserving '(env)
      get-value-code
      (make-instruction-sequence '(env val) (list target)
       `((perform (op define-variable!)
                  (const ,var)
                  (reg val)
                  (reg env))
         (assign ,target (const ok))))))))

(define (compile-if exp target linkage env)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile-exp (if-predicate exp) 'val 'next env))
            (c-code (compile-exp (if-consequent exp) target consequent-linkage env))
            (a-code (compile-exp (if-alternative exp) target linkage env)))
        (preserving '(env continue)
         p-code
         (append-instruction-sequences
          (make-instruction-sequence '(val) '()
           `((test (op false?) (reg val))
             (branch (label ,f-branch))))
          (parallel-instruction-sequences
           (append-instruction-sequences t-branch c-code)
           (append-instruction-sequences f-branch a-code))
          after-if))))))

(define (compile-sequence seq target linkage env)
  (if (last-exp? seq)
      (compile-exp (first-exp seq) target linkage env)
      (preserving '(env continue)
       (compile-exp (first-exp seq) target 'next env)
       (compile-sequence (rest-exps seq) target linkage env))))

(define (compile-lambda exp target linkage env)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage lambda-linkage
         (make-instruction-sequence '(env) (list target)
          `((assign ,target
                    (op make-compiled-procedure)
                    (label ,proc-entry)
                    (reg env)))))
        (compile-lambda-body exp proc-entry env))
       after-lambda))))

(define (compile-lambda-body exp proc-entry env)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence '(env proc argl) '(env)
      `(,proc-entry
         (assign env (op compiled-procedure-env) (reg proc))
         (assign env
                 (op extend-environment)
                 (const ,formals)
                 (reg argl)
                 (reg env))))
     (compile-sequence (lambda-body exp)
                       'val
                       'return
                       (extend-compile-time-environment formals env)))))

(define (compile-application exp target linkage env)
  (let ((proc-code (compile-exp (operator exp) 'proc 'next env))
        (operand-codes (map (lambda (operand) (compile-exp operand 'val 'next env))
                            (operands exp))))
    (preserving '(env continue)
     proc-code
     (preserving '(proc continue)
      (construct-arglist operand-codes)
      (compile-procedure-call target linkage env)))))

(define (compile-procedure-call target linkage env)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence '(proc) '()
        `((test (op primitive-procedure?) (reg proc))
          (branch (label ,primitive-branch))))
       (parallel-instruction-sequences
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl target compiled-linkage env))
        (append-instruction-sequences
         primitive-branch
         (end-with-linkage linkage
          (make-instruction-sequence '(proc argl) (list target)
           `((assign ,target
                     (op apply-primitive-procedure)
                     (reg proc)
                     (reg argl)))))))
       after-call))))

(define (compile-proc-appl target linkage env)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc) all-regs
          `((assign continue (label ,linkage))
            (assign val (op compiled-procedure-entry) (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val)) (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence '(proc) all-regs
            `((assign continue (label ,proc-return))
              (assign val (op compiled-procedure-entry) (reg proc))
              (goto (reg val))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence '(proc continue) all-regs
          `((assign val (op compiled-procedure-entry) (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE-EXP" target))
        (else (error "How did we get here?"))))

; Some of the helpers, needed in subsequent exercises

(define extra-operations
  (list (list 'lexical-address-lookup lexical-address-lookup)
        (list 'lexical-address-set! lexical-address-set!)
        (list 'the-global-environment get-global-environment)))

(define extra-registers '())

(define (make-explicit+compile-machine)
  (make-machine (append ec-registers extra-registers)
                (append cm-operations extra-operations)
                explicit+compile-text))

(define (compile-in-machine machine expression)
  (let ((instructions (assemble (statements (compile-exp expression
                                                         'val
                                                         'return
                                                         (empty-compile-time-env)))
                                machine)))
    (set-register-contents! machine 'env the-global-environment)
    (set-register-contents! machine 'val instructions)
    (set-register-contents! machine 'flag true)

    (start machine)))

(define (compiled-instructions expression)
  (statements (compile-exp expression 'val 'return (empty-compile-time-env))))
