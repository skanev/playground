; SICP exercise 2.24
;
; Suppose we evaluate the expression (list 1 (list 2 (list 3 4))). Give the
; result printed by the interpreter, the corresponding box-and-pointer
; structure, and the interpretation of this as a tree (as in Figure 2.6).

; The interpreter will print:
;
; (1 (2 (3 4)))
;
; This is the "box-and-pointer" structure. I quote it because of the ASCII.
;
;     +---+---+      +---+---+
; --> | o | o-+----> | o | / |
;     +-|-+---+      +-|-+---+
;       |              |
;       |              |
;     +---+          +---+---+      +---+---+
;     | 1 |          | o | o------> | o | / |
;     +---+          +-|-+---+      +-|-+---+
;                      |              |
;                      |              |
;                    +---+          +---+---+      +---+---+
;                    | 2 |          | o | o------> | o | / |
;                    +---+          +-|-+---+      +-|-+---+
;                                     |              |
;                                     |              |
;                                   +---+          +---+
;                                   | 3 |          | 4 |
;                                   +---+          +---+
;
; Here's the tree:
;
;               o  (1 (2 (3 4)))
;             /   \
;           /       \ 
;         1          o  (2 (3 4)
;                  /   \
;                /       \
;               2         o  (3 4)
;                       /   \
;                     /       \
;                    3         4
