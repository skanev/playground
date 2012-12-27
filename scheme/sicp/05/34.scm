; SICP exercise 5.34
;
; Compile the iterative factorial procedure
;
; (define (factorial n)
;   (define (iter product counter)
;     (if (> counter n)
;         product
;         (iter (* counter product)
;               (+ counter 1))))
;   (iter 1))
;
; Annotate the resulting code, showing the essential difference between the
; code for iterative and recursive versions of factorial that makes one
; process build up stack space and the other run in constant stack space.

(define compiled-factorial-iter
  '(
      (assign val (op make-compiled-procedure) (label entry1) (reg env))
      (goto (label after-lambda2))
    entry1
      ; (define (factorial n) ...
      (assign env (op compiled-procedure-env) (reg proc))
      (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
      (assign val (op make-compiled-procedure) (label entry3) (reg env))
      (goto (label after-lambda4))
    entry3
      ; (define (iter product counter)
      (assign env (op compiled-procedure-env) (reg proc))
      (assign env (op extend-environment) (const (product counter)) (reg argl) (reg env))
      (save continue)
      (save env)
      ; (> counter n)
      (assign proc (op lookup-variable-value) (const >) (reg env))
      (assign val (op lookup-variable-value) (const n) (reg env))
      (assign argl (op list) (reg val))
      (assign val (op lookup-variable-value) (const counter) (reg env))
      (assign argl (op cons) (reg val) (reg argl))
      (test (op primitive-procedure?) (reg proc))
      (branch (label primitive-branch8))
    compiled-branch9
      (assign continue (label after-call10))
      (assign val (op compiled-procedure-entry) (reg proc))
      (goto (reg val))
    primitive-branch8
      (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    after-call10
      (restore env)
      (restore continue)
      ; (if ...
      (test (op false?) (reg val))
      (branch (label false-branch6))
    true-branch5
      ; The base case. Here we return product
      (assign val (op lookup-variable-value) (const product) (reg env))
      (goto (reg continue))
    false-branch6
      (assign proc (op lookup-variable-value) (const iter) (reg env))
      (save continue)
      (save proc)
      (save env)
      ; (+ counter 1)
      (assign proc (op lookup-variable-value) (const +) (reg env))
      (assign val (const 1))
      (assign argl (op list) (reg val))
      (assign val (op lookup-variable-value) (const counter) (reg env))
      (assign argl (op cons) (reg val) (reg argl))
      (test (op primitive-procedure?) (reg proc))
      (branch (label primitive-branch14))
    compiled-branch15
      (assign continue (label after-call16))
      (assign val (op compiled-procedure-entry) (reg proc))
      (goto (reg val))
    primitive-branch14
      (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    after-call16
      (assign argl (op list) (reg val))
      (restore env)
      (save argl)
      ; (* counter product)
      (assign proc (op lookup-variable-value) (const *) (reg env))
      (assign val (op lookup-variable-value) (const product) (reg env))
      (assign argl (op list) (reg val))
      (assign val (op lookup-variable-value) (const counter) (reg env))
      (assign argl (op cons) (reg val) (reg argl))
      (test (op primitive-procedure?) (reg proc))
      (branch (label primitive-branch11))
    compiled-branch12
      (assign continue (label after-call13))
      (assign val (op compiled-procedure-entry) (reg proc))
      (goto (reg val))
    primitive-branch11
      (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    after-call13
      ; Here we actually execute the recursive call to iter. iter is already
      ; stored in proc
      (restore argl)
      (assign argl (op cons) (reg val) (reg argl))
      (restore proc)
      (restore continue)
      (test (op primitive-procedure?) (reg proc))
      (branch (label primitive-branch17))
    compiled-branch18
      ; This is the tail recursive part. Nothing is saved, instead the
      ; computation proceeds with the next first instruction in iter. This
      ; makes it not use up stack space.
      (assign val (op compiled-procedure-entry) (reg proc))
      (goto (reg val))
    primitive-branch17
      (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
      (goto (reg continue))
    after-call19
    after-if7
    after-lambda4
      (perform (op define-variable!) (const iter) (reg val) (reg env))
      (assign val (const ok))
      (assign proc (op lookup-variable-value) (const iter) (reg env))
      (assign val (const 1))
      (assign argl (op list) (reg val))
      (test (op primitive-procedure?) (reg proc))
      (branch (label primitive-branch20))
    compiled-branch21
      (assign val (op compiled-procedure-entry) (reg proc))
      (goto (reg val))
    primitive-branch20
      (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
      (goto (reg continue))
    after-call22
    after-lambda2
      (perform (op define-variable!) (const factorial) (reg val) (reg env))
      (assign val (const ok))
      (goto (reg continue))))

; Here we generate the compiled version.

(load-relative "showcase/compiler/helpers.scm")

(define factorial-iter-code
  '(define (factorial n)
     (define (iter product counter)
       (if (> counter n)
           product
           (iter (* counter product)
                 (+ counter 1))))
     (iter 1)))

(pretty-print (compiled-instructions factorial-iter-code))
