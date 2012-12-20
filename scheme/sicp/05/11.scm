; SICP exercise 5.11
;
; When we introduced save and restore in section 5.1.4, we didn't specify what
; would happen if you tried to restore a register that was not the last one
; saved, as in the sequence
;
; (save x)
; (save x)
; (restore y)
;
; There are several reasonable possibilities for the meaning of restore.
;
; a. (restore y) puts into y the last value saved on the stack, regardless of
; what register that value came from. This is the way our simulator behaves.
; Show how to take advantage of this behavior to eliminate one instruction
; from the Fibonacci machine of section 5.1.4 (figure 5.12).
;
; b. (restore y) puts into y the last value saved on the stack, but only if
; that value was saved from y; otherwise, it signals an error. Modify the
; simulator to behave this way. You will have to change save to put the
; register name on the stack along with the value.
;
; c. (restore y) puts into y the last value saved from y, regardless of what
; other registers were saved after y and not restored. Modify the simulator to
; behave this way. You will have to associate a separate stack with each
; register. You should make the initialize-stack operation initialize all the
; register stacks.

; Some code to allow all variants to coexist.

(define table (make-hash))

(define (make-save inst machine stack pc)
  ((hash-ref table 'make-save) inst machine stack pc))

(define (make-restore inst machine stack pc)
  ((hash-ref table 'make-restore) inst machine stack pc))

(define original-make-register make-register)

(define (make-register name)
  ((hash-ref table 'make-register) name))

(define (get-contents register)
  ((hash-ref table 'get-contents) register))

(define (set-contents! register value)
  ((hash-ref table 'set-contents!) register value))

(define (use-traditional-registers!)
  (define (get-contents register) (register 'get))
  (define (set-contents! register value) ((register 'set) value))

  (hash-set! table 'make-register original-make-register)
  (hash-set! table 'get-contents get-contents)
  (hash-set! table 'set-contents! set-contents!))

; a. Here are the modified procedures:

(define (use-version-a!)
  (define (make-save inst machine stack pc)
    (let ((reg (get-register machine (stack-inst-reg-name inst))))
      (lambda ()
        (push stack (get-contents reg))
        (advance-pc pc))))

  (define (make-restore inst machine stack pc)
    (let ((reg (get-register machine (stack-inst-reg-name inst))))
      (lambda ()
        (set-contents! reg (pop stack))
        (advance-pc pc))))

  (use-traditional-registers!)
  (hash-set! table 'make-save make-save)
  (hash-set! table 'make-restore make-restore))

; And this is the shorter Fibonacci machine:

(define (make-shorter-fibonacci-machine)
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
        (restore continue)
        (assign n (op -) (reg n) (const 2))
        (save continue)
        (assign continue (label after-fib-n-2))
        (save val)
        (goto (label fib-loop))
      after-fib-n-2
        (restore n) ; The modification is here
        (restore continue)
        (assign val (op +) (reg val) (reg n))
        (goto (reg continue))
      immediate-answer
        (assign val (reg n))
        (goto (reg continue))
      fib-done)))

; b. Erroring out when registers don't match

(define (use-version-b!)
  (define (make-save inst machine stack pc)
    (let ((reg-name (stack-inst-reg-name inst)))
      (let ((reg (get-register machine reg-name)))
        (lambda ()
          (push stack (cons reg-name (get-contents reg)))
          (advance-pc pc)))))

  (define (make-restore inst machine stack pc)
    (let ((reg-name (stack-inst-reg-name inst)))
      (let ((reg (get-register machine reg-name)))
        (lambda ()
          (let ((saved-value (pop stack)))
            (if (eq? reg-name (car saved-value))
                (begin
                  (set-contents! reg (cdr saved-value))
                  (advance-pc pc))
                (error "Mismatching registers:" (car saved-value) inst)))))))

  (use-traditional-registers!)
  (hash-set! table 'make-save make-save)
  (hash-set! table 'make-restore make-restore))

; c. A stack per each register
;
; I'm not going to modify the initialize stack operation. I can (and I
; should), but it is simpler if I don't modify the original code. I will store
; the stack in each register, since I don't want to modify make-machine. I'm
; doing this solely to have a compact exercise solution.

(define (use-version-c!)
  (define (make-register name)
    (let ((register (original-make-register name)))
      ((register 'set) (mcons '() (make-stack)))
      register))

  (define (get-contents register) (mcar (register 'get)))
  (define (set-contents! register value) (set-mcar! (register 'get) value))
  (define (push-register register value) (push (mcdr (register 'get)) value))
  (define (pop-register register) (pop (mcdr (register 'get))))

  (define (make-save inst machine stack pc)
    (let ((reg (get-register machine (stack-inst-reg-name inst))))
      (lambda ()
        (push-register reg (get-contents reg))
        (advance-pc pc))))

  (define (make-restore inst machine stack pc)
    (let ((reg (get-register machine (stack-inst-reg-name inst))))
      (lambda ()
        (set-contents! reg (pop-register reg))
        (advance-pc pc))))

  (hash-set! table 'make-register make-register)
  (hash-set! table 'get-contents get-contents)
  (hash-set! table 'set-contents! set-contents!)
  (hash-set! table 'make-save make-save)
  (hash-set! table 'make-restore make-restore))
