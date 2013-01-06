; EOPL exercise 2.13
;
; Extend the procedural representation to implement empty-env? by representing
; the environment by a list of two procedures: one that returns the value
; associated with a variable, as before, and one that returns whether or not
; the environment is empty.

(define empty-env
  (lambda ()
    (list (lambda (search-var)
            (report-no-binding-found search-var))
          (lambda () #t))))

(define extend-env
  (lambda (saved-var saved-val saved-env)
    (list (lambda (search-var)
            (if (eqv? search-var saved-var)
              saved-val
              (apply-env saved-env search-var)))
          (lambda () #f))))

(define apply-env
  (lambda (env search-var)
    ((car env) search-var)))

(define empty-env?
  (lambda (env)
    ((cadr env))))
