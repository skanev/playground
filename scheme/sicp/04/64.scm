; SICP exercise 4.64
;
; Louis Reasoner mistakenly deletes the outranked-by rule (section 4.4.1) from
; the data base. When he realizes this, he quickly reinstalls it.
; Unfortunatelly, he makes a slight change in the rule, and types it in as
;
; (rule (outranked-by ?staff-person ?boss)
;       (or (supervisor ?staff-person ?boss)
;           (and (outranked-by ?middle-manager ?boss)
;                (supervisor ?staff-person ?middle-manager))))
;
; Just after Louis types this information into the system, DeWitt Aull comes
; by to find out who outranks Ben Bitdiddle. He issues the query
;
; (outranked-by (Bitdiddle Ben) ?who)
;
; After answering, the system goes into an infinite loop. Explain why.

; First the system will answer with
;
; (outranked-by (Bitdiddle Ben) (Warbucks Oliver))
;
; because the first disjunct will match an existing assertion. Afterwards, the
; system will try to match (outranked-by ?middle-manager ?boss), which means
; applying the rule again. In this second application, we will first get all
; supervisor assertions (because they are the first clause of the disjunction),
; but when the second disjunct gets evaluated, it will invoke the rule again.
; This will get everything stuck in a loop.
