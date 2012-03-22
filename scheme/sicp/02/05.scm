; SICP exercise 2.05
;
; Show that we can represent pairs of nonnegative integers using only numbers
; and arithmetic operations if we represent the pair a and b as the integer
; that is the product 2ª3ᵇ. Give the corresponding definitions of the
; procedures cons, car and cdr.

(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (car pair)
  (count-divisor pair 2))

(define (cdr pair)
  (count-divisor pair 3))

(define (count-divisor number divisor)
  (define (iter number result)
    (if (= (remainder number divisor) 0)
        (iter (quotient number divisor) (+ result 1))
        result))
  (iter number 0))
