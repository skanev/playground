; SICP exercise 4.46
;
; The evaluators in section 4.1 and 4.2 do not determine what order operands
; are evaluated in. We will see that the amb evaluator evaluates them from
; left to right. Explain why our parsing program wouldn't work if the operands
; are evaluated in some other order.

; Well, that's painstakingly straightforward.
;
; Given the initial parse-noun-phrase definition:
;
; (define (parse-noun-phrase)
;   (list 'noun-phrase
;         (parse-word articles)
;         (parse-word nouns)))
;
; When we evaluate right to left instead of left to right, (parse-word nouns)
; would consume input before (parse-word articles), changing the meaning of
; (parse-noun-phrase) as if those two were reversed in the left to right
; evaluator.
