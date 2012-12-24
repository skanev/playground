; SICP exercise 5.20
;
; Draw the box-and-pointer representation and the memory-vector representation
; (as in figure 5.14) of the list structure produced by
;
; (define x (cons 1 2))
; (define y (list x x))
;
; with the free pointer initially p1. What is the final value of free? What
; pointers represent the values of x and y?

; The final value of free will be n4. Here are the drawings:
;
;             +---+---+       +---+---+
; (x x) ----> | o | o ------->| o | / |
;           1 +-|-+---+     2 +-|-+---+
;               |               |
;               |               V
;               |             +---+---+
;               +------------>| o | o |
;                           3 +-|-+-|-+
;                               |   |
;                               V   V
;                            +---+ +---+
;                            | 1 | | 2 |
;                            +---+ +---+
;
;    Index   0    1    2    3    4    ...
;          +----+----+----+----+----+----
; the-cars |    | p3 | p3 | n1 |    | ...
;          +----+----+----+----+----+----
; the-csrs |    | p2 | e0 | n2 |    | ...
;          +----+----+----+----+----+----
;                                ^
;                                |
;                               free
