; EOPL exercise 3.24
;
; Use the tricks of the program above to write the pair of mutually recursive
; procedures, odd and even, as in exercise 3.32.

(load-relative "cases/proc/all.scm")

(define the-program
  "let of = proc (o) proc (e) proc (n)
             if zero?(n)
             then 0
             else (((e o) e) -(n, 1))
   in let ef = proc (o) proc (e) proc (n)
                if zero? (n)
                then 1
                else (((o o) e) -(n, 1))
      in let odd = proc(n) (((of of) ef) n)
         in let even = proc(n) (((ef of) ef) n)
            in -(-((even 10), (odd 10)),
                 -((odd 11), (even 11)))")
