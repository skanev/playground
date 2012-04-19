; SICP exercise 2.49
;
; Use segments->painter to define the following primitive painters:
;
; a. The painter that draws the outline of the designated frame.
; b. The painter that draws an "X" by connecting the opposite corners of the
;    frame
; c. The painter that draws a diamond shape by connecting the mid-points of
;    the sides of the frame.
; d. The wave painter.

; Here you go.
;
; Note, that I wrote a program that takes a bunch of path coordinates and
; generates make-segment calls for each line segment in the path in order to
; implement wave.

; a. The painter that draws the outline of the designated frame.
(define outline
  (segments->painter
    (list (make-segment (make-vect 0.0 0.0) (make-vect 0.0 1.0))
          (make-segment (make-vect 0.0 1.0) (make-vect 1.0 1.0))
          (make-segment (make-vect 1.0 1.0) (make-vect 1.0 0.0))
          (make-segment (make-vect 1.0 0.0) (make-vect 0.0 0.0)))))

; b. The painter that draws an "X" by connecting the opposite corners of the
;    frame
(define cross
  (segments->painter
    (list (make-segment (make-vect 0.0 0.0) (make-vect 1.0 1.0))
          (make-segment (make-vect 1.0 0.0) (make-vect 0.0 1.0)))))

; c. The painter that draws a diamond shape by connecting the mid-points of
;    the sides of the frame.
(define diamond
  (segments->painter
    (list (make-segment (make-vect 0.5 0.0) (make-vect 1.0 0.5))
          (make-segment (make-vect 1.0 0.5) (make-vect 0.5 1.0))
          (make-segment (make-vect 0.5 1.0) (make-vect 0.0 0.5))
          (make-segment (make-vect 0.0 0.5) (make-vect 0.5 0.0)))))

; d. The wave painter.
(define wave
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
          (make-segment (make-vect 0.17 0.45) (make-vect 0.00 0.60)))))
