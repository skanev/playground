; SICP exercise 5.01
;
; Design a register machine to compute factorials using the iterative
; algorithm specified by the following procedure. Draw data-path and
; controller diagrams for this machine.
;
; (define (factorial n)
;   (define (iter product counter)
;     (if (> counter n)
;         product
;         (iter (* counter product)
;               (+ counter 1))))
;   (iter 1 1))

; Whee! More drawing! I had some fun trying to figure out how to draw this as a
; planar graph. I wonder if the exercise designers thought about that.
;
; The number we are calculating factorial of should be stored in the register
; n. When the machine finishes, the result will be stored in register p.
;
; This is the data-path diagram:
;
;                +---------+
;  +---(  >  )---|    n    |
;  |             +---------+
;  |
; +---------+  c<-1 +-----+  p<-1 +---------+
; |    c    |<-(x)-/   1   \-(x)->|    p    |
; +---------+     +---------+     +---------+
;  |   ^   |       |               |       ^
;  |   |  +---------+              |       |
;  |   |   \   +   /               |      (x) p<-*
;  |   |    +-----+                |       |
;  |  (x) c++  |                   |       |
;  |   +-------+                   |       |
;  +-----------------------+       |       |
;                          |       |       |
;                         +---------+      |
;                          \   *   /       |
;                           +-----+        |
;                              |           |
;                              +-----------+
;
; Here's the controller diagram:
;
;       start
;         |
;         V
;     +-------+
;     | c<-1  |
;     +-------+
;         |
;         V
;     +-------+
;     | p<-1  |
;     +-------+
;         |
;         V     yes
;  +-->(  >  )--------> done
;  |      |
;  |      | no
;  |      V
;  |  +-------+
;  |  | p<-*  |
;  |  +-------+
;  |      |
;  |      V
;  |  +-------+
;  |  | c++   |
;  |  +-------+
;  |      |
;  +------+
;
; Note that if you get the order of the two instructions in the loop wrong,
; the controller diagram would be wrong.
