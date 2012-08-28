; SICP exercise 3.19
;
; Redo exercise 3.18 using an algorithm that takes only a constant amount of
; space. (This requires a very clever idea.)

(require r5rs/init)

(define (has-cycle? x)
  (if (null? x)
      #f
      (let ((p1 x)
            (p2 (cdr x)))
        (define (loop)
          (cond ((null? p2) #f)
                ((null? (cdr p2)) #f)
                ((eq? p1 p2) #t)
                (else
                  (set! p1 (cdr p1))
                  (set! p2 (cddr p2))
                  (loop))))
        (loop))))

(define (lastpair x)
  (if (null? (cdr x))
      x
      (lastpair (cdr x))))

(define (make-cycle x)
  (set-cdr! (lastpair x) x)
  x)
