(require racket/gui/base)
(require racket/draw)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vectors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-vect x y)
  (list x y))

(define (xcor-vect vect)
  (car vect))

(define (ycor-vect vect)
  (cadr vect))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1)
                (xcor-vect v2))
             (+ (ycor-vect v1)
                (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1)
                (xcor-vect v2))
             (- (ycor-vect v1)
                (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Segments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-segment start end)
  (list start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cadr segment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frames
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (xcor-vect v)
                            (edge1-frame frame))
                (scale-vect (ycor-vect v)
                            (edge2-frame frame))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transforming painters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
          (make-frame new-origin
                      (sub-vect (m corner1) new-origin)
                      (sub-vect (m corner2) new-origin)))))))

(define (identity painter)
  painter)

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Composing painters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
            (transform-painter painter1
                               (make-vect 0.0 0.0)
                               split-point
                               (make-vect 0.0 1.0)))
          (paint-right
            (transform-painter painter2
                               split-point
                               (make-vect 1.0 0.0)
                               (make-vect 0.5 1.0))))

      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

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

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line-drawing painters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (draw-line
          ((frame-coord-map frame) (start-segment segment))
          ((frame-coord-map frame) (end-segment segment))))
      segment-list)))

(define outline
  (segments->painter
    (list (make-segment (make-vect 0.0 0.0) (make-vect 0.0 1.0))
          (make-segment (make-vect 0.0 1.0) (make-vect 1.0 1.0))
          (make-segment (make-vect 1.0 1.0) (make-vect 1.0 0.0))
          (make-segment (make-vect 1.0 0.0) (make-vect 0.0 0.0)))))

(define diamond
  (segments->painter
    (list (make-segment (make-vect 0.5 0.0) (make-vect 1.0 0.5))
          (make-segment (make-vect 1.0 0.5) (make-vect 0.5 1.0))
          (make-segment (make-vect 0.5 1.0) (make-vect 0.0 0.5))
          (make-segment (make-vect 0.0 0.5) (make-vect 0.5 0.0))
          )))

(define cross
  (segments->painter
    (list (make-segment (make-vect 0.0 0.0) (make-vect 1.0 1.0))
          (make-segment (make-vect 1.0 0.0) (make-vect 0.0 1.0)))))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Splitting painters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (simpler-corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (beside (below painter up)
                (below right (corner-split painter (- n 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Square of four painters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(define flipped-pairs
  (square-of-four identity flip-vert identity flip-vert))

(define (simpler-square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (simpler-corner-split painter n))))

(define (inverted-square-limit painter n)
  (let ((combine4 (square-of-four flip-vert rotate180
                                  identity flip-horiz)))
    (combine4 (corner-split painter n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Canvas-drawing functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (draw-line a b)
  (let ((original-pen (send dc get-pen)))
    (send dc set-pen "black" 1 'solid)

    (send dc draw-line (xcor-vect a) (ycor-vect a)
                       (xcor-vect b) (ycor-vect b))

    (send dc set-pen original-pen)))

(define rogers-size 150)
(define rogers-bitmap (make-object bitmap% rogers-size rogers-size))
(send rogers-bitmap load-file "rogers.jpg")

(define (rogers frame)
  (let ((original-transformation (send dc get-transformation))
        (origin (origin-frame frame))
        (x-axis (edge1-frame frame))
        (y-axis (edge2-frame frame))
        (factor (/ 1.0 (- rogers-size 2))))
    (send dc transform (vector (xcor-vect x-axis)
                               (xcor-vect y-axis)
                               (ycor-vect x-axis)
                               (ycor-vect y-axis)
                               (xcor-vect origin)
                               (ycor-vect origin)))
    (send dc scale factor factor)

    (send dc draw-bitmap rogers-bitmap 0 0)

    (send dc set-transformation original-transformation)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Drawing pictures list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pictures-in-a-row 6)
(define pictures-in-a-column 4)
(define picture-size 190)
(define picture-margin 20)

(define (draw-pictures-list pictures pos)
  (define (draw-text text x y)
    (let ((original-transformation (send dc get-transformation)))
      (send dc scale 1 -1)
      (send dc translate 0 (- 0 picture-size picture-margin))

      (send dc draw-text text x (+ (- y) 4))

      (send dc set-transformation original-transformation)))
  (if (null? pictures)
      #t
      (let ((x (+ (* (+ picture-margin picture-size)
                     (remainder pos pictures-in-a-row))
                  picture-margin))
            (y (+ (* (+ picture-margin picture-size)
                     (- pictures-in-a-column (quotient pos pictures-in-a-row) 1))
                  picture-margin))
            (label (car pictures))
            (picture (cadr pictures))
            (remaining (cddr pictures)))
        (draw-text label x y)
        (picture (make-frame (make-vect x y)
                             (make-vect picture-size 0.0)
                             (make-vect 0.0 picture-size)))
        (draw-pictures-list remaining (+ pos 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setting up a canvas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define width
  (+ (* pictures-in-a-row picture-size)
     (* pictures-in-a-row picture-margin)
     picture-margin))

(define height
  (+ (* pictures-in-a-column picture-size)
     (* pictures-in-a-column picture-margin)
     picture-margin))

(define target (make-bitmap width height))
(define dc (new bitmap-dc% [bitmap target]))

(send dc translate 0 height)
(send dc scale 1 -1)
(send dc set-smoothing 'smoothed)
(send dc set-font (make-object font% 12 'system 'normal 'bold))
(send dc set-text-foreground "dim gray")

(define frame (new frame%
                   [label "Example"]
                   [width width]
                   [height (+ height 10)]))

(define canvas
  (new canvas% [parent frame]
               [paint-callback
                 (lambda (canvas dc)
                   (send dc draw-bitmap target 0 0))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Drawing things
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(draw-pictures-list
  (list
    "outline (2.49a)"
    outline

    "cross (2.49b)"
    cross

    "diamond (2.49c)"
    diamond

    "beside"
    (beside diamond outline)

    "below (2.51)"
    (below outline cross)

    "beside, below"
    (below (beside cross outline)
           (beside diamond cross))

    "wave (2.49d)"
    wave

    "flip-horiz (2.50)"
    (flip-horiz wave)

    "rotate180 (2.50)"
    (rotate180 wave)

    "rotate270 (2.50)"
    (rotate270 wave)

    "smiling-wave (2.52a)"
    smiling-wave

    "flipped-pairs"
    (flipped-pairs wave)

    "right-split"
    (right-split outline 4)

    "up-split (2.44)"
    (up-split outline 4)

    "corner-split"
    (corner-split outline 4)

    "simpler-corner-split (2.52b)"
    (simpler-corner-split outline 4)

    "square-limit"
    (square-limit wave 4)

    "inverted-square-limit (2.52c)"
    (inverted-square-limit wave 4)

    "rogers"
    rogers

    "simpler-square-limit, rogers"
    (simpler-square-limit rogers 4)

    "square-limit, rogers"
    (square-limit rogers 4)

    "inverted-square-limit, rogers"
    (inverted-square-limit (flip-vert rogers) 4)

    "squash-inwards"
    (squash-inwards rogers)

    "square-limit, squash-inwards"
    (square-limit (squash-inwards rogers) 4))
  0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Render the frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(send frame show #t)
