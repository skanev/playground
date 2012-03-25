; SICP exercise 2.18
;
; Define a procedure reverse that takes a list as argument and returns a list
; of the same elements in reverse order:
;
; (reverse (list 1 4 9 16 25))
; (25 16 9 4 1)

; Here is a lame version:

(define (reverse items)
  (if (null? items)
      items
      (append (reverse (cdr items)) (list (car items)))))

; Here's an alternative that's way better:

(define (reverse items)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items) (cons (car items) result))))
    
  (iter items (list)))
