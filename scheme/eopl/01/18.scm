; EOPL exercise 1.18
;
; (swapper s1 s2 slist) returns a list the same as slist, but with all
; occurences of s1 replaced by s2 and all occurences of s2 replaced by s1.
;
; > (swapper 'a 'd '(a b c d))
; (d b c a)
; > (swapper 'a 'd '(a d () c d))
; (d a () c a)
; > (swapper 'x 'y '((x) y (z (x))))
; ((y) x (z (y)))

(define (swapper s1 s2 slist)
  (cond ((null? slist) '())
        ((eqv? s1 slist) s2)
        ((eqv? s2 slist) s1)
        ((symbol? slist) slist)
        (#t (map (curry swapper s1 s2) slist))))
