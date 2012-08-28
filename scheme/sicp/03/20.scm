; SICP exercise 3.20
;
; Draw environment diagrams to illustrate the evaluation of the sequence of
; expressions
;
;   (define x (cons 1 2))
;   (define z (cons x x))
;   (set-car! (cdr z) 17)
;   (car x)
;
; using the procedural representation of pairs given above. (Compare exercise
; 3.11.)

; This is the environment after the two defines:
;
; global: +-----------------------------------------------------------------+
;         | cons: <procedure>                                               |
;         | car: <procedure>                                                |
;         | cdr: <procedure>                                                |
;         | set-car!: <procedure>                                           |
;         | set-cdr!: <procedure>                                           |
;         +-----------------------------------------------------------------+
;             ^
;             |
;         +-----------------------+            +-----------------------+
;         | x: 1                  |  +---------- x: <procedure>        |
;         | y: 2                  |  |    +----- y: <procedure>        |
;         | set-x!: <procedure>   |  |    |    | set-x!: <procedure>   |
;         | set-y!: <procedure>   |  +----+    | set-y!: <procedure>   |
;         | dispatch: <procedure> |  |         | dispatch: <procedure> |
;         +---|-------------------+  |         +---|-------------------+
;             |    ^                 |             |    ^
;             |    |                 |             |    |
;         +-----------+              |         +-----------+
;  x ---> | procedure |--------------+  z ---> | procedure |
;         +-----------+                        +-----------+
;         parameters: m                        params: m
;         body: (cond ((eq? m 'car) x)         body: ...
;                     ((eq? m 'cdr) y)
;                     ((eq? m 'set-car!) set-x!)
;                     ((eq? m 'set-cdr!) set-y!)
;                     (else (error "Undefined operation - CONS" m)))
;
; This is what happens after we call (set-car! (cdr z) 17)
;
; global: +-----------------------------------------------------------------+
;         | cons: <procedure>                                               |
;         | car: <procedure>                                                |
;         | cdr: <procedure>                                                |
;         | set-car!: <procedure>                                           |
;         | set-cdr!: <procedure>                                           |
;         +-----------------------------------------------------------------+
;             ^
;             |
;         +-----------------------+            +-----------------------+
;         | x: 17                 |  +---------- x: <procedure>        |
;         | y: 2                  |  |    +----- y: <procedure>        |
;         | set-x!: <procedure>   |  |    |    | set-x!: <procedure>   |
;         | set-y!: <procedure>   |  +----+    | set-y!: <procedure>   |
;         | dispatch: <procedure> |  |         | dispatch: <procedure> |
;         +---|-------------------+  |         +---|-------------------+
;             |    ^                 |             |    ^
;             |    |                 |             |    |
;         +-----------+              |         +-----------+
;  x ---> | procedure |--------------+  z ---> | procedure |
;         +-----------+                        +-----------+
;         parameters: m                        params: m
;         body: (cond ((eq? m 'car) x)         body: ...
;                     ((eq? m 'cdr) y)
;                     ((eq? m 'set-car!) set-x!)
;                     ((eq? m 'set-cdr!) set-y!)
;                     (else (error "Undefined operation - CONS" m)))
;
; Essentially, nothing drastic happens. The only difference is where the
; variable x in the frame of the procedure named pointed by x changes from 1
; to 17.
