; EOPL exercise 2.28
;
; Write an unparser that converts the abstract syntax of an lc-exp into a
; string that matches the second grammar in this section (page 52).

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

(define (unparse exp)
  (cases lc-exp exp
    (var-exp (name)
      (symbol->string name))
    (lambda-exp (bound-var body)
      (format "(lambda (~a) ~a)" bound-var (unparse body)))
    (app-exp (rator rand)
      (format "(~a ~a)" (unparse rator) (unparse rand)))))

