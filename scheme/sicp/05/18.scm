; SICP exercise 5.18
;
; Modify the make-register procedure of section 5.2.1 so that registers can be
; traced. Registers should accept messages that turn tracing on and off. When
; a register is traced, assigning a value to the register should print the
; name of the register, the old contents of the register, and the new contents
; being assigned. Extend the interface to the machine model to permit you to
; turn tracing on and off for designated machine registers.

(define (make-register name trace)
  (let ((contents '*unassigned*)
        (tracing #f))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value)
               (when tracing
                 (trace name contents value))
               (set! contents value)))
            ((eq? message 'trace-off)
             (set! tracing #f))
            ((eq? message 'trace-on)
             (set! tracing #t))
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (make-new-machine)
  (define register-trace-proc (lambda (name old new) (void)))
  (define (trace name old new)
    (register-trace-proc name old new))
  (let ((pc (make-register 'pc trace))
        (flag (make-register 'flag trace))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops (list (list 'initialize-stack
                               (lambda () (stack 'initialize)))))
          (register-table (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table (cons (list name (make-register name trace))
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
              ((eq? message 'install-register-trace-proc)
               (lambda (proc) (set! register-trace-proc proc)))
              ((eq? message 'register-trace-off)
               (lambda (reg-name) ((lookup-register reg-name) 'trace-off)))
              ((eq? message 'register-trace-on)
               (lambda (reg-name) ((lookup-register reg-name) 'trace-on)))
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))
