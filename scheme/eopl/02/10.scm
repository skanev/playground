; EOPL exercise 2.10
;
; Add to the environment interface a constructor extend-env*, and implement it
; using the a-list representation. This constructor takes a list of variables,
; a list of values of the same length, and an environment, and is specified by
;
;   (extend-env* (varᵤ ... varᵤ) (valᵤ ... valᵤ) [f]) = [g],
;     where g(var) = { valᵢ     if var = varᵢ for some i such that 1 ≤ i ≤ u
;                    { f(val)   otherwise

(load-relative "05.scm")

(define (extend-env* vars vals env)
  (if (null? vars)
      env
      (extend-env (car vars) (car vals)
                  (extend-env* (cdr vars) (cdr vals) env))))
