; EOPL exercise 2.21
;
; Implement the data type of environments, as in section 2.2.2, using
; define-datatype. Then include has-binding? of exercise 2.9.

(define (any? obj) #t)

(define-datatype env env?
  (empty-env)
  (extend-env (var symbol?)
              (val any?)
              (enclosing env?)))

(define (has-binding? search-env search-var)
  (cases env search-env
    (empty-env () #f)
    (extend-env (var val enclosing)
                (or (eqv? var search-var)
                    (has-binding? enclosing search-var)))))
