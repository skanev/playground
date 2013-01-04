; EOPL exercise 1.06
;
; If we reversed the order of the tests in nth-element, what would go wrong?

; The code will look like this:

(define nth-element
  (lambda (lst n)
    (if (zero? n)
      (car lst)
      (if (null? lst)
        (report-list-too-short n)
        (nth-element (cdr lst) (- n 1))))))

; We will loose the error message in one specific case - that is, when we call
; (nth-elemen lst n) when n is (length lst), that is, when we try to access
; one more elements than the list has. In that case, the computation will
; eventually be reduced to a call (nth-element '() 0). Since n is zero, the
; computation will attempt to return the car of '(), which will result to an
; error. This is not the error we had in mind, though.
