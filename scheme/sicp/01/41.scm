; SICP exercise 1.41
;
; Define a procedure double that takes a procedure of one argument as argument
; and returns a procedure that applies the original procedure twice. For
; example, if inc is a procedure that adds 1 to its argument, then (double inc)
; should be a procedure that adds 2. What value is returned by

; (((double (double double)) inc) 5)
;
; (double double) returns (lambda (f) (double (double x))), which in turn
; returns a procedure, that applies f four times.
;
; (double (double double)) returns:
;
; (lambda (f) (double (double (double (double f)))))
;
; When we apply double to inc numerous times, we get:
;
; (double inc)                              +2
; (double (double inc))                     +4
; (double (double (double inc)))            +8
; (double (double (double (double inc))))   +16
;
; Thus, returns 21

(define (double function)
  (lambda (x) (function (function x))))

(define (inc x)
  (+ x 1))
