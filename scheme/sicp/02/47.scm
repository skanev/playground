; SICP exercise 2.47
;
; Here are two possible constructors for frames:
;
; (define (make-frame origin edge1 edge2)
;   (list origin edge1 edge2))
;
; (define (make-frame origin edge1 edge2)
;   (cons origin (cons edge1 edge2)))
;
; For each constructor supply the appropriate selectors to produce
; an implementation for frames.

(define (make-frame1 origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame1 frame)
  (car frame))

(define (edge1-frame1 frame)
  (cadr frame))

(define (edge2-frame1 frame)
  (caddr frame))



(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame2 frame)
  (car frame))

(define (edge1-frame2 frame)
  (cadr frame))

(define (edge2-frame2 frame)
  (cddr frame))
