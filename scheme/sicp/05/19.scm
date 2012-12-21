; SICP exercise 5.19
;
; Allysa P. Hacker wants a breakpoint feature in the simulator to help her
; debug her machine designs. You have been hired to install this feature for
; her. She wants to be able to specify a place in the controller sequence
; where the simulator will stop and allow her to examine the state of the
; machine. You are to implement a procedure
;
; (set-breakpoint <machine> <label> <n>)
;
; that sets a breakpoint just before the nth instruction after the given
; label. For example,
;
; (set-breakpoint gcd-machine 'test-b 4)
;
; installs a breakpoint in the gcd-machine just before the assignment to
; register a. When the simulator reaches the breakpoint it should print the
; label and the offset of the breakpoint and stop executing instructions.
; Alyssa can then use get-register-contents and set-register-contents! to
; manipulate the state of the simulated machine. She should then be able to
; continue execution by saying
;
; (proceed-machine <machine>)
;
; She should also be able to remove a specific breakpoint by means of
;
; (cancel-breakpoint <machine> <label> <n>)
;
; or to remove all breakpoints by means of
;
; (cancel-all-breakpoints machine)

; I will ignore the printing, since it is not that interesting. It can be
; implemented by storing (cons label offset) instead of #t in the mcaddr of
; thet instruction.
;
; Our interface for the breakpoints:

(define (set-breakpoint machine label offset)
  ((machine 'set-breakpoint) label offset))

(define (cancel-breakpoint machine label offset)
  ((machine 'cancel-breakpoint) label offset))

(define (cancel-all-breakpoints machine)
  (machine 'cancel-all-breakpoints))

(define (proceed-machine machine)
  (machine 'proceed))

; The assmebler is modified to save the labels in the machine

(define (assemble controller-text machine)
  (extract-labels controller-text
    (lambda (insts labels)
      (update-insts! insts labels machine)
      (update-labels! labels machine)
      insts)))

(define (update-labels! labels machine)
  ((machine 'install-labels) labels))

; The instruction representation stores whether there is a breakpoint on the
; instruction

(define (make-instruction text)
  (mcons text (mcons '() (mcons #f '()))))
(define (instruction-text inst)
  (mcar inst))
(define (instruction-execution-proc inst)
  (mcar (mcdr inst)))
(define (instruction-breakpoint? inst)
  (mcar (mcdr (mcdr inst))))
(define (set-instruction-execution-proc! inst proc)
  (set-mcar! (mcdr inst) proc))
(define (set-instruction-breakpoint! inst active?)
  (set-mcar! (mcdr (mcdr inst)) active?))

; The modified machine constructor

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (the-labels '()))
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
              (begin
                ((instruction-execution-proc (car insts)))
                (if (and (not (null? (get-contents pc)))
                         (instruction-breakpoint? (car (get-contents pc))))
                    'breakpoint
                    (execute))))))
      (define (set-breakpoint label offset)
        (set-instruction-breakpoint!
          (list-ref (lookup-label the-labels label) (- offset 1))
          #t))
      (define (cancel-breakpoint label offset)
        (set-instruction-breakpoint!
          (list-ref (lookup-label the-labels label) (- offset 1))
          #f))
      (define (cancel-all-breakpoints)
        (for-each (lambda (inst) (set-instruction-breakpoint! inst #f))
                  the-instruction-sequence))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'proceed)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'install-labels)
               (lambda (labels) (set! the-labels labels)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'set-breakpoint) set-breakpoint)
              ((eq? message 'cancel-breakpoint) cancel-breakpoint)
              ((eq? message 'cancel-all-breakpoints) (cancel-all-breakpoints))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))
