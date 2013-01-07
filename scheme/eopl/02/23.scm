; EOPL exercise 2.23
;
; The define of lc-exp ignores the condition in definition 1.1.8 that says
; "Identifier is any symbol other than lambda." Modify the definition of
; identifier? to capture this condition. As a hint, remember that any
; predicate can be used in define datatype, even ones you define.

(define (id? sym)
  (and (symbol? sym)
       (not (eqv? sym 'lambda))))

(define-datatype lc-exp lc-exp?
  (var-exp
    (var id?))
  (lambda-exp
    (bound-var id?)
    (body lc-exp?))
  (app-exp
    (rator lc-exp?)
    (rand lc-exp?)))
