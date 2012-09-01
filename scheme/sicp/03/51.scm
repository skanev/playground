; SICP exercise 3.51
;
; In order to take a closer look at delayed evaluation, we will use the
; following procedure, which simply returns its argument after printing it:
;
;   (define (show x)
;     (display-line x)
;     x)
;
; What does the interpreter print in response to evaluating each expression in
; the following sequence?
;
;   (define x (stream-map
;               show
;               (stream-enumerate-interval 0 10)))
;
;   (stream-ref x 5)
;   (stream-ref x 7)

; This is the output in the streams we've defined so far:
;
; (define x ..)
; 0
;
; (stream-ref x 5) ; 5
; 1
; 2
; 3
; 4
; 5
;
; (stream-ref x 7) ; 7
; 6
; 7
;
; Note, that if you run this in Racket, it will print only 5 and 7. That's
; because the cars of the streams are also lazy.
