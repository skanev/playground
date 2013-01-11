; EOPL exercise 3.01
;
; In figure 3.3, list all the places where we used the fact that
; ⎣ ⎡ n⎤ ⎦ = n.

; I will use {} for ⎣  and ⎦ and [] for ⎡  and ⎤. I will annotate the code below:
;
; Let p = [i = 1, v = 5, x = 10].
;
; (value-of
;   <<-(-(x, 3), -(v, i))>>
;   p)
;
; = [(-
;     {(value-of <<-(x, 3)>> p)}
;     {(value-of <<-(v, i)>> p)})]
;
; = [(-
;     (-
;       {(value-of <<x>> p)}
;       {(value-of <<3>> p)})
;     {(value-of <<-(v, i)>>)})]
;
; = [(-
;     (-
;       10                               ; HERE
;       {(value-of <<3>> p)})
;     {(value-of <<-(v, i)>>)})]
;
; = [(-
;     (-
;       10
;       3)                               ; HERE
;     {(value-of <<-(v, i)>>)})]
;
; = [(-
;     7
;     {(value-of <<-(v, i)>>)})]
;
; = [(-
;     7
;     (-
;       {(value-of <<x>> p)}
;       {(value-of <<i>> p)}))]
;
; = [(-
;     7
;     (-
;       5                                ; HERE
;       {(value-of <<i>> p)}))]
;
; = [(-
;     7
;     (-
;       5
;       1))]                             ; HERE
; = [(-
;     7
;     4)]
;
; = [4]
