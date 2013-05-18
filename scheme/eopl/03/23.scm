; EOPL exercise 3.23
;
; What is the value of the following PROC program?
;
;   let makemult = proc (maker)
;                   proc (x)
;                    if zero?(x)
;                    then 0
;                    else -(((maker maker) -(x, 1)), -4)
;   in let times4 = proc (x) ((makemult makemult) x)
;      in (times4 3)
;
; Use tricks of this program to write a procedure for factorial in PROC. As a
; hint, remember that you can use currying (exercise 3.20) to define a
; two-argument procedure times.

; This smells of the Y combinator. Cool. The program returns 12, as the names
; of its procedures suggest.

(load-relative "cases/proc/all.scm")

(define the-given-program
  "let makemult = proc (maker)
                   proc (x)
                    if zero?(x)
                    then 0
                    else -(((maker maker) -(x, 1)), -(0, 4))
   in let times4 = proc (x) ((makemult makemult) x)
      in (times4 3)")

(define factorial-5-program
  "let makemult = proc (maker)
                   proc (x)
                    proc (y)
                     if zero?(y)
                     then 0
                     else -((((maker maker) x) -(y, 1)), -(0, x))
   in let times = proc (x)
                   proc (y)
                    (((makemult makemult) x) y)
      in let makefact = proc (fact)
                         proc (n)
                          if zero?(n)
                          then 1
                          else ((times n) ((fact fact) -(n, 1)))
         in let factorial = proc (n)
                             ((makefact makefact) n)
            in (factorial 5)")
