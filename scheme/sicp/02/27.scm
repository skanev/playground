; SICP exercise 2.27
;
; Modify your reverse procedure in exercise 2.18 to produce a deep-reverse
; procedure that takes a list as argument and returns as its value the list
; with its elements reversed and with all sublists deep-reversed as well. For
; example,
;
; (define x (list (list 1 2) (list 3 4)))
;
; x
; ((1 2) (3 4))
;
; (reverse x)
; ((3 4) (1 2))
;
; (deep-reverse x)
; ((4 3) (2 1))

(define (deep-reverse items)
  (define (iter items result)
    (cond ((null? items) result)
          ((pair? (car items))
           (iter (cdr items) (cons (deep-reverse (car items)) result)))
          (else (iter (cdr items) (cons (car items) result)))))
    
  (iter items (list)))
