; SICP exercise 1.02
;
; Translate the following expression into prefix form:
;
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-10.html#%_thm_1.2

(/ (+ 5
      4
      (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))
