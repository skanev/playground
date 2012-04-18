; SICP exercise 2.45
;
; right-split and up-split can be expressed as instances of a general splitting
; operation. Define a procedure split with the property that evaluating
;
;   (define right-split (split beside below))
;   (define up-split (split below beside))
;
; produces right-split and up-split with the same behaviors as the ones already
; defined.

(define (split op1 op2)
  (define (split-proc painter n)
    (if (= n 0)
        painter
        (let ((smaller (split-proc painter (- n 1))))
          (op1 painter (op2 smaller smaller)))))
  split-proc)

(define right-split (split beside below))
(define up-split (split below beside))
