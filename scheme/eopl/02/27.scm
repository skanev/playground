; EOPL exercise 2.27
;
; Draw the abstract syntax tree for the lambda calculus expressions
;
; ((lambda (a) (a b)) c)
;
; (lambda (x)
;   (lambda (y)
;     ((lambda (x)
;        (x y))
;      x)))

; Expression: ((lambda (a) (a b)) c)
;
;                      +---------+
;                      | app-exp |
;                      +---------+
;                      /         \
;                  rator         rand
;                   /               \
;        +------------+             +---------+
;        | lambda-exp |             | var-exp |
;        +------------+             +---------+
;          /        \                    |
;     bound-var     body                var
;        /             \                 |
;     +---+          +---------+       +---+
;     | a |          | app-exp |       | c |
;     +---+          +---------+       +---+
;                     /       \
;                 rator       rand
;                  /             \
;           +---------+       +---------+
;           | var-exp |       | var-exp |
;           +---------+       +---------+
;                |                 |
;               var               var
;                |                 |
;              +---+             +---+
;              | a |             | b |
;              +---+             +---+
;
;
;
; Expression: (lambda (x)
;               (lambda (y)
;                 ((lambda (x)
;                    (x y))
;                  x)))
;
;         +------------+
;         | lambda-exp |
;         +------------+
;           /        \
;      bound-var     body
;        /              \
;     +---+        +------------+
;     | x |        | lambda-exp |
;     +---+        +------------+
;                    /        \
;               bound-var     body
;                 /              \
;              +---+         +---------+
;              | y |         | app-exp |
;              +---+         +---------+
;                             /       \
;                         rator       rand
;                          /             \
;              +------------+           +---------+
;              | lambda-exp |           | var-exp |
;              +------------+           +---------+
;                /        \                  |
;           bound-var     body              var
;             /              \               |
;          +---+         +---------+       +---+
;          | x |         | app-exp |       | x |
;          +---+         +---------+       +---+
;                         /       \
;                     rator       rand
;                      /             \
;              +---------+         +---------+
;              | var-exp |         | var-exp |
;              +---------+         +---------+
;                   |                   |
;                  var                 var
;                   |                   |
;                 +---+               +---+
;                 | x |               | y |
;                 +---+               +---+
