; EOPL exercise 1.20
;
; (count-occurences s slist) returns the number of occurences of s in slist.
;
; > (count-occurences 'x '((f x) y (((x z) x))))
; 3
; > (count-occurences 'x '((f x) y (((x z) () x))))
; 3
; > (count-occurences 'w '((f x) y (((x z) x))))
; 0

(define (count-occurences s slist)
  (cond ((null? slist) 0)
        ((symbol? slist) (if (eqv? s slist) 1 0))
        (#t (+ (count-occurences s (car slist))
               (count-occurences s (cdr slist))))))
