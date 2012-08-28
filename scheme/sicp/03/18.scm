; SICP exercise 3.18
;
; Write a procedure that examines a list and determines whether it contains a
; cycle, that is, whether a program that tried to find the end of the list by
; taking successive cdrs would go into an infinite loop. Exercise 3.13
; constructed such lists.

(require r5rs/init)

(define (has-cycle? x)
  (let ((counted '()))
    (define (cycle? x)
      (cond ((null? x) #f)
            ((memq x counted) #t)
            (else
              (set! counted (cons x counted))
              (cycle? (cdr x)))))
    (cycle? x)))

(define (lastpair x)
  (if (null? (cdr x))
      x
      (lastpair (cdr x))))

(define (make-cycle x)
  (set-cdr! (lastpair x) x)
  x)
