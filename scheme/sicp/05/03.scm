; SICP exercise 5.03
;
; Design a machine to compute square roots using Newton's method, as described
; in section 1.1.7:
;
; (define (sqrt x)
;   (define (good-enough? guess)
;     (< (abs (- (square guess) x)) 0.001))
;   (define (improve guess)
;     (average guess (/ x guess)))
;   (define (sqrt-iter guess)
;     (if (good-enough? guess)
;         guess
;         (sqrt-iter (improve guess))))
;   (sqrt-iter 1.0))
;
; Begin by assuming that good-enough? and improve operations are available as
; primitives. Then show how to expand these in terms of arithmetic operations.
; Describe each version of the sqrt machine design by a data-path diagram and
; writing a controller definition in the register machine language.

; Here's a bunch of helper functions:

(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
  (average guess (/ x guess)))

; Now comes the "simple" version.
;
; The data-path diagram is fairly simple:
;
;                 +-->(  ge?  )<--+
;                 |               |
;   +---+        +-----+     +-----+
;  / 1.0 \--(x)->|  g  |     |  x  |
; +-------+      +-----+     +-----+
;                 ^   |       |
;                 |  +---------+
;                 |   \  imp  /
;                 |    +-----+
;                (x)      |
;                 +-------+

(define simple-sqrt-machine
  (make-machine
    '(g x)
    (list (list 'ge? good-enough?) (list 'imp improve))
    '(
        (assign g (const 1.0))
      test-ge?
        (test (op ge?) (reg g) (reg x))
        (branch (label sqrt-done))
        (assign g (op imp) (reg g) (reg x))
        (goto (label test-ge?))
      sqrt-done)))

; This is the second version, where all we use is arithmetic operations. This
; gets a bit more involved. Let's note a few things.
;
; * I've designed the data paths to operate exclusively on register a. All
;   arithmetic operations have a as the first operand. I did that because I
;   wanted to get closer to a traditional processor. Note that some of the
;   instructions have g as the second operand, some have x, and some have a
;   constant. I did this because I don't want to overcomplicate the
;   controller.
; * Before jumping to test-good-enough?, a is already assigned the contents of
;   register g. That's why test-good-enough? comes after the a <- g
;   assignment.
; * There are, in total, ten instructions and two tests. They can be reduced
;   if we introduce some new operations (and memory), but it's not yet the
;   time for that.
;
; And now follows some ASCII art:
;
;     +-+                                +-----+
;    / 0 \---(  <  )---+  +---(  <  )---/ 0.001 \
;   +-----+            |  |            +---------+
;                      |  |
;                      |  |             +---+
;                      |  |            / 1.0 \
;                      |  |           +-------+
;                      |  |               |
;                      |  |              (x)
;                      |  |               |
;                      |  |               V
; +-------------------------+         +-------+   +-------+
; |                     a   | --(x)-> |   g   |   |   x   |
; +-------------------------+         +-------+   +-------+
;      |   |           ^  |               |           |
;      |   |    +-+    |  +----+     +----+           |
;      |   |   / 2 \   |  |    |     |    |           |
;      |   |  +-----+  |  |   +-------+   |           |
;      |   |     |     |  |    \  +  /    |           |
;      |  +-------+    |  |     +---+     |           |
;      |   \  /  /     |  |       |       |           |
;      |    +---+      +-----(x)--+       |           |
;      |      |        |  |               |           |
;  +-------+  +--(x)---+  +----+     +----+           |
;   \  -  /            |  |    |     |    |           |
;    +---+             |  |   +-------+   |           |
;      |               |  |    \  *  /    |           |
;      +---------(x)---+  |     +---+     |           |
;                      |  |       |       |           |
;                      +-----(x)--+       |           |
;                      |  |               |           |
;                      |  +----+     +----+           |
;                      |  |    |     |    |           |
;                      |  |   +-------+   |           |
;                      |  |    \  /  /    |           |
;                      |  |     +---+     |           |
;                      |  |       |       |           |
;                      +-----(x)--+       |           |
;                      |  |               |           |
;                      +-----(x)----------+           |
;                      |  |                           |
;                      |  +----+     +----------------+
;                      |       |     |                |
;                      |      +-------+               |
;                      |       \  -  /                |
;                      |        +---+                 |
;                      |          |                   |
;                      +-----(x)--+                   |
;                      |                              |
;                      +-----(x)----------------------+

(define complex-sqrt-machine
  (make-machine
    '(g x a)
    (list (list '+ +) (list '- -) (list '* *) (list '/ /) (list '< <))
    '(
        (assign g (const 1.0))
        (assign a (reg g))
      test-good-enough?
        (assign a (op *) (reg a) (reg g))
        (assign a (op -) (reg a) (reg x))
        (test (op <) (const 0) (reg a))
        (branch (label after-abs-a))
        (assign a (op -) (reg a))
      after-abs-a
        (test (op <) (reg a) (const 0.001))
        (branch (label sqrt-done))
        (assign a (reg x))
        (assign a (op /) (reg a) (reg g))
        (assign a (op +) (reg a) (reg g))
        (assign a (op /) (reg a) (const 2))
        (assign g (reg a))
        (goto (label test-good-enough?))
      sqrt-done)))
