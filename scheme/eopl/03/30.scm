; EOPL exercise 3.30
;
; What is the purpose of the call to proc-val on the next-to-last line of
; apply-env?

; It's a really simple question. Applying environments return expvals, not
; procedures. If there is no call to proc-val, then the procedure gets passed
; to expval->proc, which yields an error.
