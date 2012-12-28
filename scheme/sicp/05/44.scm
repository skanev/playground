; SICP exercise 5.44
;
; In this section we have focused on the use of the compile-time environment
; to produce lexical addresses. But there are other uses for compile-time
; environments. For instance, in exercise 5.38 we increased the efficiency of
; compiled code by open-coding primitive procedures. Our implementation
; treated the names of open-coded procedures as reserved words. If a program
; were to rebind such a name, the mechanism described in exercise 5.38 would
; still open-code it as a primitive, ignoring the new binding. For example,
; consider the procedure
;
; (lambda (+ * a b x y)
;   (+ (* a x) (* b y)))
;
; which computes a linear combination of x and y. We might call it with
; arguments +matrix, *matrix, and four matrices, but the open-coded compiler
; would still open-code the + and the * in (+ (* a x) (* b y)) as primitive +
; and *. Modify the open-coding compiler to consult the compile-time
; environment in order to compile the correct code for expressions involving
; the names of primitive procedures. (The code will work correctly as long as
; the program does not define or set! these names.)

(load-relative "43.scm")

; We just lift the code from 4.38 and modify it:

(define extra-registers '(arg1 arg2))
(define extra-operations (append extra-operations `((+ ,+) (- ,-) (* ,*) (= ,=))))
(define all-regs (append '(arg1 arg2) all-regs))

(define (open-coded? exp env)
  (and (pair? exp)
       (memq (car exp) '(+ * - =))
       (eq? (find-variable (car exp) env) 'not-found)))

(define (vararg-open-coded-exp? exp)
  (and (pair? exp)
       (memq (car exp) '(+ *))))

(define (compile-open-coded exp target linkage env)
  (when (and (not (vararg-open-coded-exp? exp))
           (not (= (length exp) 3)))
        (error "Expression should be binary" exp))
  (let ((code (car exp))
        (first-operand (cadr exp))
        (rest-operands (cddr exp)))
    (preserving '(env continue)
      (compile-exp first-operand 'arg1 'next env)
      (compile-open-coded-rest-args code rest-operands target linkage env))))

(define (compile-open-coded-rest-args code operands target linkage env)
  (if (null? (cdr operands))
      (preserving '(arg1 continue)
        (compile-exp (car operands) 'arg2 'next env)
        (end-with-linkage linkage
          (make-instruction-sequence '(arg1 arg2) (list target)
            `((assign ,target (op ,code) (reg arg1) (reg arg2))))))
      (preserving '(env continue)
        (preserving '(arg1)
          (compile-exp (car operands) 'arg2 'next env)
          (make-instruction-sequence '(arg1 arg2) '(arg1)
            `((assign arg1 (op ,code) (reg arg1) (reg arg2)))))
        (compile-open-coded-rest-args code (cdr operands) target linkage env))))

; We also need to modify compile from 4.43

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
        ((let? exp) (compile-exp (let->combination exp) target linkage env))
        ((begin? exp)
         (compile-sequence (begin-actions exp) target linkage env))
        ((cond? exp) (compile-exp (cond->if exp) target linkage env))
        ((open-coded? exp env) (compile-open-coded exp target linkage env))
        ((application? exp)
         (compile-application exp target linkage env))
        (else
         (error "Unknown expression type -- COMPILE" exp))))
