; SICP exercise 5.39
;
; Write a procedure lexical-address-lookup that implements the new lookup
; operation. It should take two arguments -- a lexical address and a run-time
; environment -- and return the value of the variable stored at the specified
; lexical address. lexical-address-lookup should signal an error if the value
; of the variable is the symbol *unassigned*. Also, write a procedure
; lexical-address-set! that implements the operation that changes the value of
; the variable at a specified lexical address.

(define (lexical-address-lookup address env)
  (define (env-ref offset env)
    (if (= offset 0)
        (frame-values (first-frame env))
        (env-ref (- offset 1) (enclosing-environment env))))

  (define (frame-ref offset vals)
    (if (= offset 0)
        (mcar vals)
        (frame-ref (- offset 1) (mcdr vals))))

  (let ((result (frame-ref (cadr address)
                           (env-ref (car address) env))))
    (if (eq? result '*unassigned*)
        (error "Unassigned variable" address)
        result)))


(define (lexical-address-set! address val env)
  (define (env-ref offset env)
    (if (= offset 0)
        (frame-values (first-frame env))
        (env-ref (- offset 1) (enclosing-environment env))))

  (define (frame-set! offset vals)
    (if (= offset 0)
        (set-mcar! vals val)
        (frame-set! (- offset 1) (mcdr vals))))

  (frame-set! (cadr address)
              (env-ref (car address) env)))
