; SICP exercise 5.05
;
; Hand-simulate the factorial and Fibonacci machines, using some nontrivial
; input (requiring execution of at least one recursive call). Show the
; contents of the stack at each significant point in the execution.

; I did that on a piece of paper. It's fairly tricky to find a way to write it
; down in this file, so I will just show the state of the stacks after various
; saves and restores.
;
; First, the factorial machine. Let's say we're calculating it for 3.
;
; Initially n is 3 and the stack is empty.
;
; 1. +-------------+  n: 3
;    |   <empty>   |  continue: fact-done
;    +-------------+  val: ?
;
; 2. +-------------+  n: 2
;    |      3      |  continue: after-fact
;    +-------------+  val: ?
;    |  fact-done  |
;    +-------------+
;
; 3. +-------------+  n: 1
;    |      2      |  continue: after-fact
;    +-------------+  val: ?
;    | after-fact  |
;    +-------------+
;    |      3      |
;    +-------------+
;    |  fact-done  |
;    +-------------+
;
; 4. +-------------+  n: 1
;    |      2      |  continue: after-fact
;    +-------------+  val: 1
;    | after-fact  |
;    +-------------+
;    |      3      |
;    +-------------+
;    |  fact-done  |
;    +-------------+
;
; 5. +-------------+  n: 2
;    |      3      |  continue: after-fact
;    +-------------+  val: 2
;    |  fact-done  |
;    +-------------+
;
; 6. +-------------+  n: 3
;    |   <empty>   |  continue: fact-done
;    +-------------+  val: 6
;
;
;
; The Fibonacci machine is a bit more intricate:
;
;  1. +---------------+  n: 3
;     |    <empty>    |  continue: fib-done
;     +---------------+  val: ?
;
;  2. +---------------+  n: 2
;     |       3       |  continue: after-fib-n-1
;     +---------------+  val: ?
;     |   fib-done    |
;     +---------------+
;
;  3. +---------------+  n: 1
;     |       2       |  continue: after-fib-n-1
;     +---------------+  val: ?
;     | after-fib-n-1 |
;     +---------------+
;     |       3       |
;     +---------------+
;     |   fib-done    |
;     +---------------+
;
;  4. +---------------+  n: 1
;     |       2       |  continue: after-fib-n-1
;     +---------------+  val: 1
;     | after-fib-n-1 |
;     +---------------+
;     |       3       |
;     +---------------+
;     |   fib-done    |
;     +---------------+
;
;  5. +---------------+  n: 0
;     |       1       |  continue: after-fib-n-2
;     +---------------+  val: 1
;     | after-fib-n-1 |
;     +---------------+
;     |       3       |
;     +---------------+
;     |   fib-done    |
;     +---------------+
;
;  6. +---------------+  n: 0
;     |       1       |  continue: after-fib-n-2
;     +---------------+  val: 0
;     | after-fib-n-1 |
;     +---------------+
;     |       3       |
;     +---------------+
;     |   fib-done    |
;     +---------------+
;
;  7. +---------------+  n: 0
;     |       3       |  continue: after-fib-n-1
;     +---------------+  val: 1
;     |   fib-done    |
;     +---------------+
;
;  8. +---------------+  n: 1
;     |       1       |  continue: after-fib-n-2
;     +---------------+  val: 1
;     |   fib-done    |
;     +---------------+
;
;  9. +---------------+  n: 1
;     |       1       |  continue: after-fib-n-2
;     +---------------+  val: 1
;     |   fib-done    |
;     +---------------+
;
; 10. +---------------+  n: 1
;     |    <empty>    |  continue: fib-done
;     +---------------+  val: 2
