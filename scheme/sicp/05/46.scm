; SICP exercise 5.46
;
; Carry out an analysis like the one in exercise 5.45 to determine the
; effectiveness of compiling the tree-recursive Fibonacci procedure
;
; (define (fib n)
;   (if (< n 2)
;       n
;       (+ (fib (- n 1)) (fib (- n 2)))))
;
; compared to the effectiveness of using the special-purpose Fibonacci machine
; of figure 5.12. (For measurement of the interpreted performance, see
; exercise 5.29). For Fibonacci, the time resource used is not linear in n;
; hence the ratios of stack operations will not approach a limiting value that
; is independent of n.

; Here are the results in a table (this time generated with Racket):
;
; +--------+------------------------+-----------------------+
; |        |       total-pushes     |     maximum-depth     |
; |        +------+-----+-----+-----+-----+-----+-----+-----+
; |        | int  | cmp | opc | sh  | int | cmp | opc | sht |
; +--------+------+-----+-----+-----+-----+-----+-----+-----+
; | fib(1) |   16 |   7 |   7 |   0 |   8 |   3 |   3 |   0 |
; | fib(2) |   72 |  17 |  15 |   3 |  13 |   5 |   4 |   2 |
; | fib(3) |  128 |  27 |  23 |   6 |  18 |   8 |   6 |   4 |
; | fib(4) |  240 |  47 |  39 |  12 |  23 |  11 |   8 |   6 |
; | fib(5) |  408 |  77 |  63 |  21 |  28 |  14 |  10 |   8 |
; | fib(6) |  688 | 127 | 103 |  36 |  33 |  17 |  12 |  10 |
; | fib(7) | 1136 | 207 | 167 |  60 |  38 |  20 |  14 |  12 |
; | fib(8) | 1864 | 337 | 271 |  99 |  43 |  23 |  16 |  14 |
; | fib(9) | 3040 | 547 | 439 | 162 |  48 |  26 |  18 |  16 |
; +--------+------+-----+-----+-----+-----+-----+-----+-----+
; Legend: * int - interpreted
;         * cmp - compiled with the 5.5 compiler
;         * opc - compiled with open-coding primitives
;         * sht - special hand-tailored version
;
; If we stare at the total pushes for a while, we figure out that:
; - int grows with 56 * fib(n) on each iteration
; - cmp grows with 10 * fib(n) on each iteration
; - opc grows with  8 * fib(n) on each iteration
; - sht grows with  3 * fib(n) on each iteration
;
; As for the maximum-depth:
; - int takes 5n + 3
; - cmp takes 3n - 1 (except for 1 and 2)
; - opc takes 2n     (except for 1 and 2)
; - sht takes 2n - 2
;
; It is worth noting that this time opc is around 2.66 slower than sht (in
; comparison to the previous exercise) because the exponential growth of the
; function makes those extra saves and restores really count.

(require (prefix-in srfi: srfi/48))
(load-relative "showcase/compiler/helpers.scm")
(load-relative "tests/helpers/monitored-stack.scm")

; This is the recursive procedure:

(define code
  '(define (fib n)
     (if (< n 2)
       n
       (+ (fib (- n 1)) (fib (- n 2))))))

; Let's use the 5.06 version of the Fibonacci machine

(define sht-machine
  (make-machine
    '(n val continue)
    (list (list '< <) (list '- -) (list '+ +))
    '(
        (assign continue (label fib-done))
      fib-loop
        (test (op <) (reg n) (const 2))
        (branch (label immediate-answer))
        (save continue)
        (assign continue (label after-fib-n-1))
        (save n)
        (assign n (op -) (reg n) (const 1))
        (goto (label fib-loop))
      after-fib-n-1
        (restore n)
        (assign n (op -) (reg n) (const 2))
        (assign continue (label after-fib-n-2))
        (save val)
        (goto (label fib-loop))
      after-fib-n-2
        (assign n (reg val))
        (restore val)
        (restore continue)
        (assign val (op +) (reg val) (reg n))
        (goto (reg continue))
      immediate-answer
        (assign val (reg n))
        (goto (reg continue))
      fib-done)))

; Code that gathers stats

(define (gather-stats machine name callback)
  (printf "~a:\n" name)
  (for/list ([n (in-range 1 10)])
    (set-register-contents! machine 'flag false)
    (let ((stats (callback n)))
      (printf "  fib(~a) takes ~a\n" n stats)
      (list (third stats) (sixth stats)))))

(define (build-machine)
  (make-machine (append '(arg1 arg2) ec-registers)
                (append `((+ ,+) (- ,-) (* ,*) (= ,=)) cm-operations)
                explicit+compile-text))

; Stats for interpretation

(define int-machine (build-machine))
(eval-in-machine int-machine code)
(define int-stats
  (gather-stats int-machine
                "Interpreted code"
                (lambda (n) (stack-stats-for int-machine (list 'fib n)))))

; Stats for compilation

(define cmp-machine (build-machine))
(compile-in-machine cmp-machine code)
(define cmp-stats
  (gather-stats cmp-machine
                "Compiled code"
                (lambda (n) (stack-stats-for cmp-machine (list 'fib n)))))

; Stats for compilation with open-coding primitives

(load-relative "38.scm")
(define opc-machine (build-machine))
(compile-in-machine opc-machine code)
(define opc-stats
  (gather-stats opc-machine
                "Compiled code with open-coding"
                (lambda (n) (stack-stats-for opc-machine (list 'fib n)))))

; Stats for the special hand-tailored version

(define sht-stats
  (gather-stats sht-machine
                "Special hand-tailored version"
                (lambda (n)
                  (set-register-contents! sht-machine 'n n)
                  ((sht-machine 'stack) 'initialize)
                  (start sht-machine)
                  ((sht-machine 'stack) 'statistics))))

; Printing the results in a nice table

(define (pad n . args)
  (srfi:format "~3F" n))

(newline)
(printf "The final results:\n")
(printf "+--------+------------------------+-----------------------+\n")
(printf "|        |       total-pushes     |     maximum-depth     |\n")
(printf "|        +------+-----+-----+-----+-----+-----+-----+-----+\n")
(printf "|        | int  | cmp | opc | sh  | int | cmp | opc | sht |\n")
(printf "+--------+------+-----+-----+-----+-----+-----+-----+-----+\n")
(for ([n (in-range 1 10)]
      [int int-stats]
      [cmp cmp-stats]
      [opc opc-stats]
      [sht sht-stats])
     (printf "| fib(~a) | ~a | ~a | ~a | ~a | ~a | ~a | ~a | ~a |\n"
             n
             (srfi:format "~4F" (first int)) (pad (first cmp)) (pad (first opc)) (pad (first sht))
             (pad (second int)) (pad (second cmp)) (pad (second opc)) (pad (second sht))))
(printf "+--------+------+-----+-----+-----+-----+-----+-----+-----+\n")
