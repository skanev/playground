; SICP exercise 5.38
;
; Our compiler is clever about avoiding unnecessary stack operations, but it
; is not clever at all when it comes to compiling calls to the primitive
; procedures of the language in terms of the primitive operations supplied by
; the machine. For example, consider how much code is compiled to compute
; (+ a 1): The code sets up an argument list in argl, puts the primitive
; addition procedure (which it finds by lookup up the symbol + in the
; environment) into proc, and tests whether the procedure is primitive or
; compound. The compiler always generates code to perform the test, as well as
; code for primitive and compound branches (only one of which will be
; executed). We have not shown the part of the controller that implements
; primitives, but we presume that these instructions make use of primitive
; arithmetic operations in the machine's data path. Consider how much less
; code would be generated if the compiler could open-code primitives -- that
; is, if it could generate code to directly use these primitive machine
; operations. The expression (+ a 1) might be compiled into something as
; simple as
;
; (assign val (op lookup-variable-value) (const a) (reg env))
; (assign val (op +) (reg val) (const 1))
;
; In this exercise, we will extend our compiler to support open coding of
; selected primitives. Special-purpose code will be generated fo calls to
; these primitive procedures instead of the general proedure-application code.
; In order to support this, we will augment our machine with special argument
; registers arg1 and arg2. The primitive arithmetic operations of the machine
; will take their inputs from arg1 and arg2. The results may be put into val,
; arg1, or arg2.
;
; The compiler must be able to recognize the application of an open-coded
; primitive in the source program. We will augment the dispatch in the compile
; procedure to recognize the names of these primitives in addition to the
; reserved words (the special forms) it currently recognizes. For each special
; form our compiler has a code generator. In this exercise we will construct a
; family of code generators for the open-coded primitives.
;
; a. The open-coded primitives, unlike the special forms, all need their
; operands evaluated. Write a code generator spread-arguments for use by all
; the open-coding code generators. spread-arguments should take an operand
; list and compile the given operands targeted to successive argument
; registers. Note that an operand may contain a call to an open-coded
; primitive, so argument registers will have to be preserved during operand
; evaluation.
;
; b. For each of the primitive procedures =, *, -, and +, write a code
; generator that takes a combination with that operator, together with a
; target and a linkage descriptor, and produces code to spread the arguments
; into the registers and then performs the operation targeted to the given
; target with the given linkage. You need only handle expressions with two
; operands. Make compile dispatch to these code generators.
;
; c. Try your new compiler on the factorial example. Compare the resulting
; code with the result produced without open coding.
;
; d. Extend your code generators for + and * so that they can handle
; expressions with arbitrary numbers of operands. An expression with more
; than two operands will have to be compiled into a sequence of operations,
; each with only two inputs.

; I will just ignore the spread-arguments nonsense, since I cannot figure out
; how to get it working with the results I want.
;
; What I want is the expression (+ 1 2 3) to be compiled to:
;
;   (assign arg1 (const 1))
;   (assign arg2 (const 2))
;   (assign arg1 (op +) (reg arg1) (reg arg2))
;   (assign arg2 (const 3))
;   (assign val (op +) (reg arg1) (reg arg2))
;
; That is, the first operand gets assign to arg1 and every other gets assigned
; to arg2 and subsequently added to arg1.

; The compiled factorial is below. The differences are highlighted.

(define compiled-factorial
  '(  (assign val (op make-compiled-procedure) (label entry1) (reg env))
      (goto (label after-lambda2))
    entry1
      (assign env (op compiled-procedure-env) (reg proc))
      (assign env (op extend-environment) (const (n)) (reg argl) (reg env))

      ; <changed>
      ; We don't need to do a call here, which removes a bunch of instructions
      ; and a save/restore of continue and env
      (assign arg1 (op lookup-variable-value) (const n) (reg env))
      (assign arg2 (const 1))
      (assign val (op =) (reg arg1) (reg arg2))
      ; </changed>

      (test (op false?) (reg val))
      (branch (label false-branch4))
    true-branch3
      (assign val (const 1))
      (goto (reg continue))
    false-branch4

      ; <changed>
      ; We skip another call, which saves a save/restore of proc and argl and
      ; another bunch of instruction
      (save continue)
      (save env) ; Saving env happens here, instead of when entering the procedure
      (assign proc (op lookup-variable-value) (const factorial) (reg env))
      (assign arg1 (op lookup-variable-value) (const n) (reg env))
      (assign arg2 (const 1))
      (assign val (op -) (reg arg1) (reg arg2))
      ; </changed>

      (assign argl (op list) (reg val))
      (test (op primitive-procedure?) (reg proc))
      (branch (label primitive-branch6))
    compiled-branch7
      (assign continue (label proc-return9)) ; Different return label
      (assign val (op compiled-procedure-entry) (reg proc))
      (goto (reg val))

    proc-return9
      ; This is different, since we store result in arg1, not val.
      (assign arg1 (reg val))
      (goto (label after-call8))

    primitive-branch6
      (assign arg1 (op apply-primitive-procedure) (reg proc) (reg argl))
    after-call8

      ; <changed>
      ; We save another call, including a save/restore of argl.
      (restore env)
      (restore continue)
      (assign arg2 (op lookup-variable-value) (const n) (reg env))
      (assign val (op *) (reg arg1) (reg arg2))
      ; </changed>

      (goto (reg continue))
    after-if5
    after-lambda2
      (perform (op define-variable!) (const factorial) (reg val) (reg env))
      (assign val (const ok))))

; I need to modify compile-exp to check if an operation should be open-coded.

(define (compile-exp exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage))
        ((assignment? exp)
         (compile-assignment exp target linkage))
        ((definition? exp)
         (compile-definition exp target linkage))
        ((if? exp) (compile-if exp target linkage))
        ((lambda? exp) (compile-lambda exp target linkage))
        ((begin? exp)
         (compile-sequence (begin-actions exp) target linkage))
        ((cond? exp) (compile-exp (cond->if exp) target linkage))
        ((open-coded? exp) (compile-open-coded exp target linkage))
        ((application? exp)
         (compile-application exp target linkage))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

; I need to add arg1 and arg2 to all-regs, so they will be preserved when
; there is a function call.

(define all-regs (append '(arg1 arg2) all-regs))

; Methods to check open coding

(define (open-coded? exp)
  (and (pair? exp)
       (memq (car exp) '(+ * - =))))

(define (vararg-open-coded-exp? exp)
  (and (pair? exp)
       (memq (car exp) '(+ *))))

; The real work

(define (compile-open-coded exp target linkage)
  (when (and (not (vararg-open-coded-exp? exp))
           (not (= (length exp) 3)))
        (error "Expression should be binary" exp))
  (let ((code (car exp))
        (first-operand (cadr exp))
        (rest-operands (cddr exp)))
    (preserving '(env continue)
      (compile-exp first-operand 'arg1 'next)
      (compile-open-coded-rest-args code rest-operands target linkage))))

(define (compile-open-coded-rest-args code operands target linkage)
  (if (null? (cdr operands))
      (preserving '(arg1 continue)
        (compile-exp (car operands) 'arg2 'next)
        (end-with-linkage linkage
          (make-instruction-sequence '(arg1 arg2) (list target)
            `((assign ,target (op ,code) (reg arg1) (reg arg2))))))
      (preserving '(env continue)
        (preserving '(arg1)
          (compile-exp (car operands) 'arg2 'next)
          (make-instruction-sequence '(arg1 arg2) '(arg1)
            `((assign arg1 (op ,code) (reg arg1) (reg arg2)))))
        (compile-open-coded-rest-args code (cdr operands) target linkage))))

(define factorial-code
  '(define (factorial n)
     (if (= n 1)
         1
         (* (factorial (- n 1)) n))))

;(pretty-print (statements (compile-exp factorial-code 'val 'next)))
