; SICP exercise 5.10
;
; Design a new syntax for register-machine instructions and modify the
; simulator to use your new syntax. Can you implement your new syntax without
; changing any part of the simulator except procedures in this section?

; Sure.
;
; Registers will be prefixed with @ and labels will be prefixed with :.
; Numbers will be constants. Everything else is an operation.

(define (symbol-starting-with? symbol prefix)
  (and (symbol? symbol)
       (equal? (substring (symbol->string symbol) 0 (string-length prefix))
               prefix)))

(define (symbol-without-prefix symbol)
  (string->symbol (substring (symbol->string symbol) 1)))

(define (register-exp? exp) (symbol-starting-with? exp "@"))
(define (register-exp-reg exp) (symbol-without-prefix exp))
(define (constant-exp? exp) (number? exp))
(define (constant-exp-value exp) exp)
(define (label-exp? exp) (symbol-starting-with? exp ":"))
(define (label-exp-label exp) (symbol-without-prefix exp))
(define (operation-exp? exp)
  (and (pair? exp)
       (not (register-exp? (car exp)))
       (not (constant-exp? (car exp)))
       (not (label-exp? (car exp)))))
(define (operation-exp-op operation-exp) (car operation-exp))
(define (operation-exp-operands operation-exp) (cdr operation-exp))

(define fibonacci-machine
  (make-machine
    '(n val continue)
    (list (list '< <) (list '- -) (list '+ +))
    '(
        (assign continue :fib-done)
      fib-loop
        (test < @n 2)
        (branch :immediate-answer)
        (save continue)
        (assign continue :after-fib-n-1)
        (save n)
        (assign n - @n 1)
        (goto :fib-loop)
      after-fib-n-1
        (restore n)
        (restore continue)
        (assign n - @n 2)
        (save continue)
        (assign continue :after-fib-n-2)
        (save val)
        (goto :fib-loop)
      after-fib-n-2
        (assign n @val)
        (restore val)
        (restore continue)
        (assign val + @val @n)
        (goto @continue)
      immediate-answer
        (assign val @n)
        (goto @continue)
      fib-done)))
