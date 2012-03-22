; SICP exercise 2.06
;
; In case representing pairs as procedures wasn't mind-boggling enough,
; consider that, in a language that can manipulate procedures, we can get by
; without numbers (at least insofar as nonnegative integers are concerned) by
; implementing 0 and the operation of adding 1 as
;
; (define zero (lambda (f) (lambda (x) x)))
;
; (define (add-1 n)
;   (lambda (f) (lambda (x) (f ((n f) x)))))
;
; This representation is known as Church numerals, after its inventor, Alonzo
; Church, the logician who invented the λ calculus.
;
; Define one and two directly (not in terms of zero and add-1). (Hint: Use
; substitution to evaluate (add-1 zero)). Give a direct definition of the
; addition procedure (not in terms of repeated application of add-1).

; It was mind-boggling enough, but I am going to do the exercise anyway.
;
; This is one:
;
; (add-1 zero)
; (add-1 (lambda (f) (lambda (x) x)))
; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
; ...which essentially is...
; (lambda (f) (lambda (x) (f x)))

(define one
  (lambda (f) (lambda (x) (f x))))

; And this is two:
;
; (add-1 one)
; (add-1 (lambda (f) (lambda (x) (f x))))
; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
; ...which essentially is...
; (lambda (f) (lambda (x) (f (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))

; Therefore, to add a to b, we have to apply f a + b times.

(define (add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))
