; SICP exercise 3.63
;
; Louis Reasoner asks why the sqrt-stream procedure was not written in the
; following more straightforward way, without the local variable guesses:
;
;   (define (sqrt-stream x)
;     (cons-stream 1.0
;                  (stream-map (lambda (guess)
;                                (sqrt-improve guess x))
;                              (sqrt-stream x))))
;
; Alyssa P. Hacker replies that this version of the procedure is considerably
; less efficient because it performs redundant computation. Explain Alyssa's
; answer. Would the two versions still differ in efficiency if our
; implementation of delay used only (lambda () <exp>) without using the
; optimization provided by mem-proc (section 3.5.1)?

; Since sqrt-stream is dependend on itself, every calculation of the (n + 1)th
; term would require calculating the nth term again. This is exponential. By
; using a variable, we're reusing the stream and we don't need to recalculate
; the previous terms. If our delay does not memoize, the two versions would be
; equally slow.
