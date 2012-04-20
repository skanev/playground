; SICP exercise 2.55
;
; Eva Lu Ator types to the interpreter the expression
;
; (car ''abracadabra)
;
; To her surprise, the interpreter prints back quote. Explain.

; Simple.
;
; 'foo  is short for (quote foo)
; ''foo is short for (quote (quote foo))
;
; When you (car (quote (quote foo))) you get 'quote.
