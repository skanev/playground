; SICP exercise 5.33
;
; Consider the following definition of a factorial procedure, which is
; slightly different frmo the one given above:
;
; (define (factorial-alt n)
;   (if (= n 1)
;       1
;       (* n (factorial-alt (- n 1)))))
;
; Compile this procedure and compare the resulting code with that produced for
; factorial. Explain the differences you find. Does either program execute
; more efficiently?

; This is the diff:
;
; 33c33,35
; <   (save env)
; ---
; >   (assign val (op lookup-variable-value) (const n) (reg env))
; >   (assign argl (op list) (reg val))
; >   (save argl)
; 61,63c63
; <   (assign argl (op list) (reg val))
; <   (restore env)
; <   (assign val (op lookup-variable-value) (const n) (reg env))
; ---
; >   (restore argl)
;
; The new version of factorial needs to save and restore env (in order to
; evaluate n after the recursive call), while the original needs to save and
; restore argl (in order to put in it the value of the recursive call). Apart
; from that, the instructions are pretty much the same. There is no difference
; in efficiency.

(load-relative "showcase/compiler/helpers.scm")

(define factorial-alt-code
  '(define (factorial-alt n)
     (if (= n 1)
       1
       (* n (factorial-alt (- n 1))))))

(pretty-print (compiled-instructions factorial-alt-code))
