; SICP exercise 1.27
;
; Demonstrate that the Carmichael numbers listed in footnote 1.17 really do
; fool the Fermat test. That is, write a procedure that takes an integer n and
; tests whether aⁿ is congruent to a modulo n for every a < n, and try your
; procedure on the given Carmichael numbers.

; Just use the function carmichael?, although do note, that it should be named
; carmichael-or-prime?

(define (carmichael? number)
  (define (congruent-to-number-below a)
    (cond ((= a 1) #t)
          ((= (expmod a number number) (remainder a number))
           (congruent-to-number-below (- a 1)))
          (else #f)))

  (congruent-to-number-below (- number 1)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (square n)
  (* n n))
