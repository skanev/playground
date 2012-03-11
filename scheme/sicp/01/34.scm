; SICP exercise 1.34
;
; Suppose we define the procedure
;
; (define (f g)
;   (g 2))
;
; Then we have
;
; (f square)
; 4
; 
; (f (lambda (z) (* z (+ z 1))))
; 6
; 
; What happens if we (perversely) ask the interpreter to evaluate the
; combination (f f)? Explain.

; We get an error. In Racket it looks like this:
;
; procedure application: expected procedure, given: 2; arguments were: 2
;
; It is fairly obvious why it happens. Let's expand it:
;
; (f f)
; (f 2)
; (2 2)
;
; It ends up invoking 2 as a procedure, when it is not really a procedure.
