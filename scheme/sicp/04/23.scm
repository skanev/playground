; SICP exercise 4.23
;
; Alyssa P. Hacker doesn't understand why analyze-sequence needs to be so
; complicated. All the other analysis procedures are straightforward
; transformations of the corresponding evaluation procedures (or eval clauses)
; in section 4.1.1. She expected analyze-sequence to look like this:
;
;   (define (analyze-sequence exps)
;     (define (execute-sequence procs env)
;       (cond ((null? (cdr procs)) ((car procs) env))
;             (else ((car procs) env)
;                   (execute-sequence (cdr procs) env))))
;     (let ((procs (map analyze exps)))
;       (if (null? procs)
;           (error "Empty sequence -- ANALYZE"))
;       (lambda (env) (execute-sequence procs env))))
;
; Eva Lu Ator explains to Alyssa that the version in the text does more of the
; work of evaluating a sequence at analysis time. Alyssa's sequence-execution
; procedure, rather than having the calls to the individual built in, loops
; through the procedures in order to call them: In effect, although the
; individual expressions in the sequence have been analyzed, the sequence
; itself has not been.
;
; Compare the two versions of analyze-sequence. For example, consider the
; common case (typical of procedure bodies) where the sequence has just one
; expression. What work will the execution procedure produced by Alyssa's
; program do? What about the execution procedure produced by the program in
; the text above? How do the two versions compare for a sequence with two
; expressions?

; The extra work is iterating the over each expression in the body. In the
; case of a body with one expression, Alyssa's code will do an additional
; cond, null?, cdr and car. In case of a procedure with two expressions in its
; body, Alyssa's code will in addition do another cond, null?, car and two
; extra cdrs (one for the null? check and one in the else clause).
