; SICP exercise 4.71
;
; Louis Reasoner wonders why the simple-query and disjoin procedures (section
; 4.4.4.2) are implemented using explicit delay operations, rather than being
; defined as follows:
;
; (define (simple-query query-pattern frame-stream)
;   (stream-flatmap
;     (lambda (frame)
;       (stream-append (find-assertions query-pattern frame)
;                      (apply-rules query-pattern frame)))
;     frame-stream))
; (define (disjoin disjuncts frame-stream)
;   (if (empty-disjunction? disjuncts)
;       the-empty-stream
;       (interleave
;         (qeval (first-disjunct disjuncts) frame-stream)
;         (disjoin (rest-disjuncts disjuncts) frame-stream))))
;
; Can you give examples of queries where these simpler definitions would lead
; to undesirable behavior?

; Assuming the implementation of streams suggested in the book, the second
; argument to both function will get evaluated before calling the function,
; which might lead to infinite recursions. To be fair, this has only a single
; disadvantage - using the delayed versions we can get some results printed,
; before the interpreter gets stuck in an infinite loop. With this adjustment,
; we won't get any results before getting stuck.
;
; An example is the following rule:
;
; (something a)
; (rule (something ?a)
;       (something ?a))
;
; And the query is:
;
; (something ?what)
