; SICP exercise 5.43
;
; We argued in section 4.1.6 that internal definitions for block structure
; should not be considered "real" defines. Rather, a procedure body should be
; interpreted as if the internal variables being defined were installed as
; ordinary lambda variables initialized to their correct values using set!.
; Section 4.1.6 and exercise 4.16 showed how to modify the metacircular
; interpreter to accomplish this by scanning out internal definitions. Modify
; the compiler to perform the same transformations before it compiles a
; procedure body.

(load-relative "42.scm")

; First, here is the support for let.

(define (let? exp) (tagged-list? exp 'let))

(define (let->combination exp)
  (let ((names (map car (cadr exp)))
        (values (map cadr (cadr exp)))
        (body (cddr exp)))
    (cons (cons 'lambda (cons names body))
          values)))

; Second, here is scan-out-defines.

(define (scan-out-defines body)
  (define (definitions-in body)
    (cond ((null? body)
           '())
          ((definition? (car body))
           (cons (car body) (definitions-in (cdr body))))
          (else
           (definitions-in (cdr body)))))
  (define (body-without-definitions body)
    (cond ((null? body)
           '())
          ((definition? (car body))
           (body-without-definitions (cdr body)))
          (else
           (cons (car body) (body-without-definitions (cdr body))))))
  (define (definition->unassigned-pair definition)
    (list (definition-variable definition) ''*unassigned*))
  (define (definition->set! definition)
    (list 'set! (definition-variable definition) (definition-value definition)))
  (define (defines->let definitions body)
    (list
      (cons 'let
            (cons (map definition->unassigned-pair definitions)
                  (append (map definition->set! definitions) body)))))

  (let ((internal-definitions (definitions-in body)))
    (if (null? internal-definitions)
        body
        (defines->let internal-definitions (body-without-definitions body)))))

; This time we hook it to lambda-body

(define (lambda-body exp) (scan-out-defines (cddr exp)))

; We also need to modify compile in order to pick up lets

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
        ((application? exp)
         (compile-application exp target linkage env))
        (else
         (error "Unknown expression type -- COMPILE" exp))))
