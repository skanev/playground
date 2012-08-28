; SICP exercise 3.17
;
; Devise a correct version of count-pairs procedure of exercise 3.16 that
; retuns the number of distinct pairs in any structure. (Hint: traverse the
; structure, maintaining an auxiliary data structure that is used to keep
; track of which pairs have already been counted.)

(define (count-pairs x)
  (let ((counted '()))
    (define (count x)
      (cond ((not (pair? x)) 0)
            ((null? x) 0)
            ((memq x counted) 0)
            (else
              (set! counted (cons x counted))
              (+ 1
                 (count (car x))
                 (count (cdr x))))))
    (count x)))
