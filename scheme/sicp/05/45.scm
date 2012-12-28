; SICP exercise 5.45
;
; By comparing the stack operations used by compiled code to the stack
; operations used by the evaluator for the same computation, we can determine
; the extent to which the compiler optimizes use of the stack, both in speed
; (reducing the total number of stack operations) and in space (reducing the
; maximum stack depth). Comparing this optimized stack use to performance of a
; special-purpose machine for the same computation gives some indication of
; the quality of the compiler.
;
; a. Exercise 5.27 asked you to determine, as a function of n, the number of
; pushes and the number of maximum stack depth needed by the evaluator to
; compute n! using the recursive factorial procedure given above. Exercise
; 5.14 asked you to do the same measurements for the special-purpose factorial
; machine shown in figure 5.11. Now perform the same analysis using the
; compiled factorial procedure.
;
; Take the ratio of the number of pushes in the compiled version to the number
; of pushes in the interpreted version, and do the same for the maximum stack
; depth. Since the number of operations and the stack depth used to compute n!
; are linear in n, these ratios should approach constants as n becomes large.
; What are these constants? Similarly, find the ratios of the stack usage in a
; special-purpose machine to the usage in the interpreted version.
;
; Compare the ratios for the special-purpose versus interpreted code to the
; ratios for compiled versus interpreted code. You should find that the
; special-purpose machine does much better than the compiled code, since the
; hand-tailored controller code should be much better than what is produced by
; our rudimentary general-purpose compiler.
;
; b. Can you suggest improvements to the compiler that would help it generate
; code that would come closer in performance to the hand-tailored version?

; a. Let's compare both the open-coding compiler and the simpler one.
;
; Without open-coding optimizations:
;   1! takes (total-pushes = 7 maximum-depth = 3)
;   2! takes (total-pushes = 13 maximum-depth = 5)
;   3! takes (total-pushes = 19 maximum-depth = 8)
;   4! takes (total-pushes = 25 maximum-depth = 11)
;   5! takes (total-pushes = 31 maximum-depth = 14)
;   6! takes (total-pushes = 37 maximum-depth = 17)
;   7! takes (total-pushes = 43 maximum-depth = 20)
;   8! takes (total-pushes = 49 maximum-depth = 23)
;   9! takes (total-pushes = 55 maximum-depth = 26)
; With open-coding optimizations:
;   1! takes (total-pushes = 5 maximum-depth = 3)
;   2! takes (total-pushes = 7 maximum-depth = 3)
;   3! takes (total-pushes = 9 maximum-depth = 4)
;   4! takes (total-pushes = 11 maximum-depth = 6)
;   5! takes (total-pushes = 13 maximum-depth = 8)
;   6! takes (total-pushes = 15 maximum-depth = 10)
;   7! takes (total-pushes = 17 maximum-depth = 12)
;   8! takes (total-pushes = 19 maximum-depth = 14)
;   9! takes (total-pushes = 21 maximum-depth = 16)
;
; As usual, code to reproduce is below.
;
; Now we can do a table
; +----+-----------------------+-----------------------+
; |    |      total-pushes     |     maximum-depth     |
; |    +-----+-----+-----+-----+-----+-----+-----+-----+
; |    | int | cmp | opc | sht | int | cmp | opc | sht |
; +----+-----+-----+-----+-----+-----+-----+-----+-----+
; | 1! |  16 |   7 |   5 |   0 |   8 |   3 |   3 |   0 |
; | 2! |  48 |  13 |   7 |   2 |  13 |   5 |   3 |   2 |
; | 3! |  80 |  19 |   9 |   4 |  18 |   8 |   4 |   4 |
; | 4! | 112 |  25 |  11 |   6 |  23 |  11 |   6 |   6 |
; | 5! | 144 |  31 |  13 |   8 |  28 |  14 |   8 |   8 |
; | 6! | 176 |  37 |  15 |  10 |  33 |  17 |  10 |  10 |
; | 7! | 208 |  43 |  17 |  12 |  38 |  20 |  12 |  12 |
; | 8! | 240 |  49 |  19 |  14 |  43 |  23 |  14 |  14 |
; | 9! | 272 |  55 |  21 |  16 |  48 |  26 |  16 |  16 |
; +----+-----+-----+-----+-----+-----+-----+-----+-----+
; Legend: * int - interpreted
;         * cmp - compiled with the 5.5 compiler
;         * opc - compiled with open-coding primitives
;         * sht - special hand-tailored version
;
; We can compare ratios by comparing the ratio of the differences between
; computing n! and (n + 1)!
;
; total pushes:
;  int / cmp is 32 / 6 ≈ 5.333
;  int / opc is 32 / 2 = 16.0
;  cmp / sht is  6 / 2 = 3.0
;  opc / sht is  2 / 2 = 1.0
;
; That is, the compiled code is 5.3 times faster than the interpreted (16
; times if open-coding instructions) and the hand-tailored version is 3 times
; faster than the copmiled (or as fast with the hand-tailored version).
;
; maximum-depth
;   int / cmp is 5 / 3 ≈ 1.666
;   int / opc is 5 / 2 = 2.5
;   cmp / sht is 3 / 2 = 1.5
;   opc / sht is 2 / 2 = 1.0
;
; That is, the compiled code uses 1.66 less space than the interpreted (2.5
; times less if open-coding instructions) and the hand-tailored version uses
; 1.5 less space than the compiled (or as much if open-coding instructions).
;
; Note that we're speaking asymptotically and we're ignoring the number of
; performed instructions as opposed to checking stack pushes.
;
; b. Open-coding comes pretty near. Of course, this assumes that the
; instruction count does not matter. There are two thinks we can do to get
; even closer.
;
; First, we can do away with storing variables in environments and just use
; the registers. That way we will eliminate environment lookup for n and
; factorial.
;
; Second, we can replace the check if factorial is a primitive procedure with
; a jump to the beginning of the function.
;
; Those two along with open-coding will come to pretty much the same code as
; the hand-tailored version.

(load-relative "showcase/compiler/helpers.scm")
(load-relative "tests/helpers/monitored-stack.scm")

(define code
  '(define (factorial n)
     (if (= n 1)
       1
       (* (factorial (- n 1)) n))))

(define (report-stats)
  (define machine (make-machine (append '(arg1 arg2) ec-registers)
                                (append `((+ ,+) (- ,-) (* ,*) (= ,=)) cm-operations)
                                explicit+compile-text))

  (compile-in-machine machine code)

  (for ([n (in-range 1 10)])
    (set-register-contents! machine 'flag false)
    (printf "  ~a! takes ~a\n" n (stack-stats-for machine (list 'factorial n)))))

(printf "Without open-coding optimizations:\n")
(report-stats)

(load-relative "38.scm")
(printf "With open-coding optimizations:\n")
(report-stats)
