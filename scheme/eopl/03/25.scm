; EOPL exercise 3.25
;
; The tricks of the previous exercises can be generalized to show that we can
; define any recursive procedure in PROC. Consider the following bit of code:
;
; let makerec = proc (f)
;                let d = proc (x)
;                         proc (z) ((f (x x)) z)
;                in proc (n) ((f (d d)) n)
; in let maketimes4 = proc (f)
;                      proc (x)
;                       if zero?(x)
;                       then 0
;                       else -((f -(x, 1)), -(0, 4))
;    in let times4 = (makerec maketimes4)
;       in (times4 3)
;
; Show that it returns 12.

; This is the Y-combinator. I'm showing that it returns 12 by running it in
; the tests.

(load-relative "cases/proc/all.scm")

(define the-example-program
  "let makerec = proc (f)
                  let d = proc (x)
                           proc (z) ((f (x x)) z)
                  in proc (n) ((f (d d)) n)
   in let maketimes4 = proc (f)
                        proc (x)
                         if zero?(x)
                         then 0
                         else -((f -(x, 1)), -(0, 4))
      in let times4 = (makerec maketimes4)
         in (times4 3)")
