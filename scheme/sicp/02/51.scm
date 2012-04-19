; SICP exercise 2.51
;
; Define the below operations for painters. below takes two painters as
; arguments. The resulting painter, given the frame, draws the first painter in
; the bottom of the frame and with the second painter on the top.  Define below
; in two different ways - first by writing a procedure that is analogous to the
; beside procedure given above, and again in terms of beside and suitable
; rotation operations.

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
            (transform-painter painter1
                               (make-vect 0.0 0.0)
                               (make-vect 1.0 0.0)
                               split-point))
          (paint-top
            (transform-painter painter2
                               split-point
                               (make-vect 1.0 0.5)
                               (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

(define (below2 painter1 painter2)
  (rotate270 (beside (rotate90 painter1)
                     (rotate90 painter2))))
