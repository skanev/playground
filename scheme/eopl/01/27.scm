; EOPL exercise 1.27
;
; (flatten slist) returns a list of the symbols contained in slist in the
; order in which they occur when slist is printed. Intuitively, flatten
; removes all the inner parentheses from its arguments.
;
; > (flatten '(a b c))
; (a b c)
; > (flatten '((a) () (b ()) () (c)))
; (a b c)
; > (flatten '((a b) c (((d)) e)))
; (a b c d e)
; > (flatten '(a b (() (c))))
; (a b c)

(define (flatten slist)
  (cond ((null? slist) '())
        ((pair? slist) (append (flatten (car slist))
                               (flatten (cdr slist))))
        (#t (list slist))))
