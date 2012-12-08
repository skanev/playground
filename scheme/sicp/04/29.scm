; SICP exercise 4.29
;
; Exhibit a program that you would expect to run much more slowly without
; memoization than with memoization. Also, consider the following interaction,
; where the id procedure is defined as in exercise 4.27 and count starts as 0:
;
;   (define (square x)
;     (* x x))
;
; ;;; L-Eval input:
; (square (id 10))
; ;;; L-Eval output:
; <response>
; ;;; L-Eval input:
; count
; ;;; L-Eval output:
; <response>

; The interaction would go as follows:
;
; ;;; L-Eval input:
; (square (id 10))
; ;;; L-Eval output:
; 100
; ;;; L-Eval input:
; count
; ;;; L-Eval output:
; 2
;
; The result from square is obvious. Count is 2, because (id 10) got evaluated
; twice - once for each argument of the multiplication.
;
; We had a simple example of such a function in exercise 1.20:
;
; (define (gcd a b)
;   (if (= b 0)
;       a
;       (gcd b (remainder a b))))
;
; Check out the exercise to see the expansion. The thunk b is evaluated twice
; for every application of gcd, which will take much more time than with
; memoization.
