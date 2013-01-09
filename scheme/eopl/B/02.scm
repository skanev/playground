; EOPL exercise B.02
;
; Why can't the grammar above be written with separated-list?

; There are at least a few reasons. For one, it would allow empty expressions
; which is something we don't want. Second, the separator needs to be a
; terminal, otherwise SLLGEN just chokes. We can rewrite it if we introduce
; add-exp, sub-exp, mul-exp and div-exp.
