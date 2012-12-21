; SICP exercise 5.16
;
; Augment the simulator to provide for instruction tracing. That is, before
; each instruction is executed, the simulator should print the text of the
; instruction. Make the machine model accept trace-on and trace-off messages
; to turn tracing on and off.

; I will base this on the solution of the previous exercise, since I'm going
; to need both instruction counting and tracing for the next one. Instead of
; printing the instructions, I will allow providing a trace procedure so
; testing can be easier.

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (trace-proc (lambda (inst) (void)))
        (tracing #f)
        (instruction-count 0))
    (let ((the-ops (list (list 'initialize-stack
                               (lambda () (stack 'initialize)))))
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
              (let ((inst (car insts)))
                (when tracing (trace-proc (instruction-text inst)))
                ((instruction-execution-proc inst))
                (set! instruction-count (+ instruction-count 1))
                (execute)))))
      (define (get-instruction-count)
        (let ((count instruction-count))
          (set! instruction-count 0)
          count))
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
              ((eq? message 'instruction-count) (get-instruction-count))
              ((eq? message 'install-trace-proc)
               (lambda (proc) (set! trace-proc proc)))
              ((eq? message 'trace-on) (set! tracing #t))
              ((eq? message 'trace-off) (set! tracing #f))
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))
