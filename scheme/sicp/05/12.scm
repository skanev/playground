; SICP exercise 5.12
;
; The simulator can be used to help determine the data paths required for
; implementing a machine with a given controller. Extend the assembler to
; store the following information in the machine model:
;
; * a list of all instructions, with duplicates removed, sorted by instruction
;   type (assign, goto, and so on);
; * a list (without duplicates) of the registers used to hold entry points
;   (these are the registers referenced by goto instructions);
; * a list (without duplicates) of the registers that are saved or restored;
; * for each register, a list (without duplicates) of the sources from which
;   it is assigned (for example, the sources for register val in the factorial
;   of figure 5.11 are (const 1) and ((op *) (reg n) (reg val))).
;
; Extend the message-passing interface to the machine to provide access to
; this new information. To test your analyzer, define the Fibonacci machine
; from figure 5.12 and examine the lists you constructed.

(define (extract-data-path-info controller-text)
  (list (list 'instructions (data-path-instructions controller-text))
        (list 'entry-point-registers (data-path-entry-point-registers controller-text))
        (list 'stack-registers (data-path-stack-registers controller-text))
        (list 'register-sources (data-path-register-sources controller-text))))

(define (data-path-instructions controller-text)
  (process-text controller-text
                (lambda (inst) #t)
                (lambda (inst) inst)))

(define (data-path-entry-point-registers controller-text)
  (process-text controller-text
                (lambda (inst) (and (eq? (car inst) 'goto)
                                    (register-exp? (goto-dest inst))))
                (compose goto-dest register-exp-reg)))

(define (data-path-stack-registers controller-text)
  (process-text controller-text
                (lambda (inst) (or (eq? (car inst) 'save)
                                   (eq? (car inst) 'restore)))
                stack-inst-reg-name))

(define (data-path-register-sources controller-text)
  (define (to-alist items result)
    (cond ((null? items) (list result))
          ((null? result)
           (to-alist (cdr items) (car items)))
          ((eq? (caar items) (car result))
           (to-alist (cdr items) (cons (car result)
                                      (append (cdr result)
                                              (list (cadar items))))))
          (else (cons result (to-alist items '())))))

  (to-alist
    (process-text controller-text
                  (lambda (inst) (eq? (car inst) 'assign))
                  (lambda (inst) (list (assign-reg-name inst)
                                       (if (operation-exp? (assign-value-exp inst))
                                           (assign-value-exp inst)
                                           (car (assign-value-exp inst))))))

    '()))

(define (process-text controller-text predicate proc)
  (sort (remove-duplicates (map proc
                                (filter predicate
                                        (filter pair?
                                                controller-text))))

        string<?
        #:key (lambda (inst) (format "~a" inst))
        #:cache-keys? #f))

; Tweaks for make-machine and make-new-machine:

(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    ((machine 'install-data-path-info)
     (extract-data-path-info controller-text))
    machine))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (data-path-info '()))
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
              ((eq? message 'install-data-path-info)
               (lambda (info) (set! data-path-info info)))
              ((eq? message 'data-path-instructions)
               (cadr (assoc 'instructions data-path-info)))
              ((eq? message 'data-path-entry-point-registers)
               (cadr (assoc 'entry-point-registers data-path-info)))
              ((eq? message 'data-path-stack-registers)
               (cadr (assoc 'stack-registers data-path-info)))
              ((eq? message 'data-path-register-sources)
               (cadr (assoc 'register-sources data-path-info)))
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))
