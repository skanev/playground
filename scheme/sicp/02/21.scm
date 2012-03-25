; SICP exercise 2.21
;
; The procedure square-list takes a list of numbers as arguments and returns a
; list of the squares of those numbers.
;
; (square-list (list 1 2 3 4))
; (1 4 9 16)
;
; Here are two different definitions of square-list. Complete both of the by
; filling in the missing expressions:
;
; (define (square-list items)
;   (if (null? items)
;       nil
;       (cons <??> <??>)))
;
; (define (square-list items)
;   (map <??> <??>))

(define (square-list-1 items)
  (if (null? items)
      '()
      (cons (square (car items))
            (square-list-1 (cdr items)))))

(define (square-list-2 items)
  (map square items))

(define (square x)
  (* x x))
