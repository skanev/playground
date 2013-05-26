; EOPL exercise 3.37
;
; With dynamic binding (exercise 3.28), recursive procedures may be bound by
; let; no special mechanism is necessary for recursion. This is of historical
; interest; in the early years of programming language design other approaches
; to recursion, such as those discussed in section 3.4, were not widely
; understood. To demonstrate recursion via dynamic binding, test the program
;
;   let fact = proc (n) add(1)
;   in let fact = proc (n)
;                  if zero?(n)
;                  then 1
;                  else *(n, (fact -(n, 1)))
;      in (fact 5)
;
; using both lexical and dynamic binding. Write the mutually recursive
; procedures even and odd as in section 3.4 in the defined language with
; dynamic binding.

(load-relative "28.scm")

(define the-program
  "let odd  = proc (n) if zero?(n) then 0 else (even -(n, 1))
   in let even = proc (n) if zero?(n) then 1 else (odd -(n, 1))
      in (odd 11)")
