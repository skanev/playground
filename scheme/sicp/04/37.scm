; SICP exercise 4.37
;
; Ben Bitdiddle claims that the following method for generating Pythagorean
; triples is more efficient than the one in exercise 4.35. Is he correct?
; (Hint: Consider the number of possibilities that must be explored.)
;
;   (define (a-pythagorean-triple-between low high)
;     (let ((i (an-integer-between low high))
;           (hsq (* high high)))
;       (let ((j (an-integer-between i high)))
;         (let ((ksq (+ (* i i) (* j j))))
;           (require (>= hsq ksq))
;           (let ((k (sqrt ksq)))
;             (require (integer? k))
;             (list i j k))))))

; It seems so. Ben's version is n², while the text version is n³. On the other
; hand, Ben's version uses sqrt which is slower in general, so there might be
; cases where where Ben's version is slower (probably for small values of
; high - low).
