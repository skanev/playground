; EOPL exercise 2.14
;
; Extend the representation of the preceding exercise to include a third
; procedure that implements has-binding? (see exercise 2.9).

(define empty-env
  (lambda ()
    (list (lambda (search-var)
            (report-no-binding-found search-var))
          (lambda () #t)
          (lambda (search-var) #f))))

(define extend-env
  (lambda (saved-var saved-val saved-env)
    (list (lambda (search-var)
            (if (eqv? search-var saved-var)
              saved-val
              (apply-env saved-env search-var)))
          (lambda () #f)
          (lambda (search-var)
            (if (eqv? search-var saved-var)
              #t
              (has-binding? saved-env search-var))))))

(define apply-env
  (lambda (env search-var)
    ((car env) search-var)))

(define empty-env?
  (lambda (env)
    ((cadr env))))

(define has-binding?
  (lambda (env search-var)
    ((caddr env) search-var)))
