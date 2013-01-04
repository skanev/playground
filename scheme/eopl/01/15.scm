; EOPL exercise 1.15
;
; (duple n x) returns a list containing n copies of x.
;
; > (duple 2 3)
; (3 3)
; > (duple 4 '(ha ha))
; ((ha ha) (ha ha) (ha ha) (ha ha))
; > (duple 0 '(blah))
; '()

; duple: Int × Any → Listof(Any)
; usage: takes an argument item and returns a list of containing n copies of
;        item.
(define duple
  (lambda (count item)
    (if (zero? count)
      '()
      (cons item (duple (- count 1) item)))))
