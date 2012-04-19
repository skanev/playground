; SICP exercise 2.52
;
; Make changes to the square limit of wave show in Figure 2.9 by working at
; each of the levels described above. In particular:
;
; a. Add some segments to the primitive wave painter of Exercise 2.49 (to add a
;    smile, for example).
; b. Change the pattern constructed by corner-split (for example, by using one
;    copy of the up-split and right-split images instead of two).
; c. Modify the version of square-limit that uses square-of-four as to assemble
;    the corners in a different pattern. (For example, you might make the big
;    Mr. Rogers look outward from each corner of the square)

; a. Add some segments to the primitive wave painter of Exercise 2.49 (to add a
;    smile, for example).
(define smiling-wave
  (segments->painter
    (list (make-segment (make-vect 0.00 0.70) (make-vect 0.16 0.57))
          (make-segment (make-vect 0.16 0.57) (make-vect 0.30 0.67))
          (make-segment (make-vect 0.30 0.67) (make-vect 0.37 0.67))
          (make-segment (make-vect 0.37 0.67) (make-vect 0.40 0.64))
          (make-segment (make-vect 0.40 0.64) (make-vect 0.42 0.68))
          (make-segment (make-vect 0.42 0.68) (make-vect 0.32 0.80))
          (make-segment (make-vect 0.32 0.80) (make-vect 0.33 0.85))
          (make-segment (make-vect 0.33 0.85) (make-vect 0.36 1.00))

          (make-segment (make-vect 0.60 1.00) (make-vect 0.62 0.84))
          (make-segment (make-vect 0.62 0.84) (make-vect 0.62 0.78))
          (make-segment (make-vect 0.62 0.78) (make-vect 0.53 0.70))
          (make-segment (make-vect 0.53 0.70) (make-vect 0.57 0.64))
          (make-segment (make-vect 0.57 0.64) (make-vect 0.63 0.67))
          (make-segment (make-vect 0.63 0.67) (make-vect 0.68 0.66))
          (make-segment (make-vect 0.68 0.66) (make-vect 0.87 0.51))
          (make-segment (make-vect 0.87 0.51) (make-vect 1.00 0.40))

          (make-segment (make-vect 1.00 0.30) (make-vect 0.73 0.52))
          (make-segment (make-vect 0.73 0.52) (make-vect 0.61 0.53))
          (make-segment (make-vect 0.61 0.53) (make-vect 0.67 0.25))
          (make-segment (make-vect 0.67 0.25) (make-vect 0.71 0.00))

          (make-segment (make-vect 0.60 0.00) (make-vect 0.56 0.23))
          (make-segment (make-vect 0.56 0.23) (make-vect 0.51 0.28))
          (make-segment (make-vect 0.51 0.28) (make-vect 0.46 0.28))
          (make-segment (make-vect 0.46 0.28) (make-vect 0.40 0.12))
          (make-segment (make-vect 0.40 0.12) (make-vect 0.36 0.00))

          (make-segment (make-vect 0.23 0.00) (make-vect 0.34 0.30))
          (make-segment (make-vect 0.34 0.30) (make-vect 0.36 0.52))
          (make-segment (make-vect 0.36 0.52) (make-vect 0.32 0.55))
          (make-segment (make-vect 0.32 0.55) (make-vect 0.28 0.55))
          (make-segment (make-vect 0.28 0.55) (make-vect 0.17 0.45))
          (make-segment (make-vect 0.17 0.45) (make-vect 0.00 0.60))

          (make-segment (make-vect 0.41 0.78) (make-vect 0.54 0.78))
          (make-segment (make-vect 0.54 0.78) (make-vect 0.52 0.76))
          (make-segment (make-vect 0.52 0.76) (make-vect 0.43 0.76))
          (make-segment (make-vect 0.43 0.76) (make-vect 0.41 0.78)))))

; b. Change the pattern constructed by corner-split (for example, by using one
;    copy of the up-split and right-split images instead of two).
(define (simpler-corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (beside (below painter up)
                (below right (corner-split painter (- n 1)))))))

; c. Modify the version of square-limit that uses square-of-four as to assemble
;    the corners in a different pattern. (For example, you might make the big
;    Mr. Rogers look outward from each corner of the square)
(define (inverted-square-limit painter n)
  (let ((combine4 (square-of-four flip-vert rotate180
                                  identity flip-horiz)))
    (combine4 (corner-split painter n))))
