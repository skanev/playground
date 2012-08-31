; SICP exercise 3.36
;
; Suppose we evaluate the following sequence of expressions in the global
; environment:
;
;   (define a (make-connector))
;   (define b (make-connector))
;   (set-value! a 10 'user)
;
; At some time during evaluation of the set-value!, the following expression
; from the connector's local procedure is evaluated:
;
;   (for-each-except
;     setter inform-about-value constraints)
;
; Draw an environment diagram showing the environment in which the above
; expression is evaluated

; globals:
; +-------------------------------------------------------------------------+
; | a: <procedure>                                                          |
; | b: <procedure>                                                          |
; | make-connector: <procedure>                                             |
; | inform-about-value: <procedure>                                         |
; | ...                                                                     |
; +-------------------------------------------------------------------------+
;                ^
;                |
; +------------------------------+
; | set-my-value: <procedure>    |
; | forget-my-value: <procedure> |
; | connect: <procedure>         |
; | me: <procedure>              |
; +------------------------------+
;                ^
;                |
; +------------------------------+
; | value: false                 |
; | informant: false             |
; | constraints: ()              |
; +------------------------------+
;                ^
;                |
; +------------------------------+
; | newval: 10                   |
; | setter: 'user                |
; +------------------------------+
