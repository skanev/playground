; EOPL exercise 1.07
;
; The error message form nth-element is uninformative. Rewrite nth-element so
; that it produces a more informative error message such as "(a b c) does not
; have 8 elements"

; I don't know how much Scheme can I use in this exercise, so I will just go
; with an internal definition.

(define (nth-element lst n)
  (define (iter items counter)
    (if (null? items)
      (eopl:error 'nth-element "~s does not have ~s elements." lst n)
      (if (zero? counter)
        (car items)
        (iter (cdr items) (- counter 1)))))
  (iter lst n))
