; SICP exercise 5.14
;
; Measure the number of pushes and the maximum stack depth required to compute
; n! for various small values of n using the factorial machine shown in figure
; 5.11. From your data determine formulas in terms of n for the total number
; of push operations and the maximum stack depth used in computing n! for any
; n > 1. Note that each of these is a linear function of n and is thus
; determined by two constants. In order to get the statistics printed, you
; will have to augment the factorial machine with instructions to initialize
; the stack and print the statistics. You may want to also modify the machine
; so that it repeatedly reads a value for n, computes the factorial, and
; prints the result (as we did for the GCD machine in figure 5.4), so that you
; will not have to repeatedly invoke get-register-contents,
; set-register-contents!, and start.

; The results are:
;
;   Running 1!: (total-pushes = 0 maximum-depth = 0)
;   Running 2!: (total-pushes = 2 maximum-depth = 2)
;   Running 3!: (total-pushes = 4 maximum-depth = 4)
;   Running 4!: (total-pushes = 6 maximum-depth = 6)
;   Running 5!: (total-pushes = 8 maximum-depth = 8)
;   Running 6!: (total-pushes = 10 maximum-depth = 10)
;   Running 7!: (total-pushes = 12 maximum-depth = 12)
;   Running 8!: (total-pushes = 14 maximum-depth = 14)
;   Running 9!: (total-pushes = 16 maximum-depth = 16)
;
; This implies that for computing n!, there are in total 2(n - 1) pushes. This
; number is equal to the maximum depth too.

(load-relative "tests/helpers/simulator.scm")

; The modified procedures:

(define (make-stack)
  (let ((s '())
       (number-pushes 0)
       (max-depth 0)
       (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth (- current-depth 1))
            top)))
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (display (list 'total-pushes '= number-pushes
                     'maximum-depth '= max-depth))
      (newline))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            ((eq? message 'print-statistics) (print-statistics))
            (else (error "Unknown request -- STACK" message))))
    dispatch))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops (list (list 'initialize-stack
                               (lambda () (stack 'initialize)))
                         (list 'print-stack-statistics
                               (lambda () (stack 'print-statistics)))))
          (register-table (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table (cons (list name (make-register name))
                                       register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register: " name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define factorial-machine
  (make-machine
    '(n val continue)
    (list (list '= =) (list '- -) (list '* *))
    '(
        (perform (op initialize-stack))
        (assign continue (label fact-done))
      fact-loop
        (test (op =) (reg n) (const 1))
        (branch (label base-case))
        (save continue)
        (save n)
        (assign n (op -) (reg n) (const 1))
        (assign continue (label after-fact))
        (goto (label fact-loop))
      after-fact
        (restore n)
        (restore continue)
        (assign val (op *) (reg n) (reg val))
        (goto (reg continue))
      base-case
        (assign val (const 1))
        (goto (reg continue))
      fact-done
        (perform (op print-stack-statistics)))))

(define (measure-factorial n)
  (set-register-contents! factorial-machine 'n n)
  (display "Running ")
  (display n)
  (display "!: ")
  (start factorial-machine))

(for ([n (in-range 1 10)])
  (measure-factorial n))
