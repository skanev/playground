; SICP exercise 4.47
;
; Louis Reasoner suggests that, since a verb phrase is either a verb or a verb
; phrase followed by a prepositional phrase, it would be much more
; straightforward to define the procedure parse-verb-phrase as follows (and
; similarly for noun phrases):
;
;   (define (parse-verb-phrase)
;     (amb (parse-word verbs)
;          (list 'verb-phrase
;                (parse-verb-phrase)
;                (parse-prepositional-phrase))))
;
; Does this work? Does the program's behavior change if we interchange the
; order of expressions in amb?

; It does not work. The problem is apparent when we rearrange the statements.
; parse-verb-phrase falls into an infinite recurssion.
;
; When the expressions in amb are in the order Louis' suggets, it might
; produce a result if (parse-word verbs) matches anything, but calling
; try-again will get it stuck in the other infinite recursion branch. That
; might also produce a result, but eventually, trying to exhaust all
; posibilities will be stuck in that recursion.
