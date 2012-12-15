; SICP exercise 4.70
;
; What is the purpose of the let bindings in the procedures add-assertion! and
; add-rule! ? What would be wrong with the following implementation of
; add-assertion! ? Hint: Recall the definition of the infinite stream of ones
; in section 3.5.2: (define ones (cons-stream 1 ones)).
; 
; (define (add-assertion! assertion)
;   (store-assertion-in-index assertion)
;   (set! THE-ASSERTIONS
;         (cons-stream assertion THE-ASSERTIONS))
;   'ok)

; If we define add-assertion! that way, THE-ASSERTIONS will effectively be a
; stream containing infinitely many times the passed assertion. The reason is
; that THE-ASSERTIONS is evaluated in a lazy way.
