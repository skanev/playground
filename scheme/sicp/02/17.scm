; SICP exercise 2.17
;
; Define a procedure last-pair that returns the list that contains only the last
; element of a given (nonempty) list:
;
; (list-pair (list 23 72 149 34))
; (34)

(define (last-pair items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))
