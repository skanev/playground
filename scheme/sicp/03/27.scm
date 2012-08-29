; SICP exercise 3.27
;
; Memoization (also called tabulation) is a technique that enables a procedure
; to record, in a local table, values that have previously been computed. This
; technique can make a vast difference in the performance of a program. A
; memoized procedure matintains a table in which values of previous calls are
; stored using as keys the arguments that produced the values. When the
; memoized procedure is asked to compute a value, it first checks the table to
; see if the value is already there and, if so, just returns that value.
; Otherwise, it compute sthe new value in the ordinary way and stores this in
; the table. As an example of memoization, recall from section 1.2.2 the
; exponential process for computing Fibonacci numbers:
;
;   (define (fib n)
;     (cond ((=n 0) 0)
;           ((=n 1) 1)
;           (else (+ (fib (- n 1))
;                    (fib (- n 2))))))
;
; The memoized version of the same procedure is
;
;   (define memo-fib
;     (memoize (lambda (n)
;                (cond ((= n 0) 0)
;                      ((= n 1) 1)
;                      (else (+ (memo-fib (- n 1))
;                               (memo-fib (- n 2))))))))
;
; where the memoizer is defined as
;
;   (define (memoize f)
;     (let ((table (make-table)))
;       (lambda (x)
;         (let ((previously-computed-result (lookup x table)))
;           (or previously-computed-result
;               (let ((result (f x)))
;                 (insert! x result table)
;                 result))))))
;
; Draw an environment diagram to analyze the computation (memo-fib 3). Explain
; why memo-fib computes the nth Fibonacci number in a number of steps
; proportional to n. Would the scheme still work if we had simply defined
; memo-fib to be (memoize fib)?

; God, I hate environment diagrams. Here's how the environment looks before
; computing (memo-fib 3).
;
; globals:
; +-------------------------------------------------------------------------+
; | memoize: <procedure>                                                    |
; | memo-fib: <procedure>                                                   |
; +--|----------------------------------------------------------------------+
;    |            ^
;    |            |
;    |   +----------------+
;    |   | f: <procedure> --------> <lambda>
;    |   +----------------+         params: n
;    |            ^                 body: (cond ((= n 0) 0)
;    |            |                             ...
;    |   +----------------+
;    |   | table: <table> --------> (*table)
;    |   +----------------+
;    |            ^
;    |            |
;    +------> <lambda>
;             parameters: x
;             body: (let ((...
;
; When we call (memo-fib 3) it does a table lookup, fails and then goes to
; calculate [1] (+ (memo-fib 2) (memo-fib 1)). Let's assume left-to-right
; evaluation. It procedures to evaluate (memo-fib 2) which fails the table
; lookup and then results to [2] (+ (memo-fib 1) (memo-fib 0)). Both fail the
; lookup, but at least they return immediatelly and the results are written in
; the table. This is how the environment looks when [2] completes:
;
; globals:
; +-------------------------------------------------------------------------+
; | memoize: <procedure>                                                    |
; | memo-fib: <procedure>                                                   |
; +--|----------------------------------------------------------------------+
;    |            ^
;    |            |
;    |   +----------------+
;    |   | f: <procedure> --------> <lambda>
;    |   +----------------+         params: n
;    |            ^                 body: (cond ((= n 0) 0)
;    |            |                             ...
;    |   +----------------+
;    |   | table: <table> --------> (*table (1 . 1)
;    |   +----------------+                 (0 . 0))
;    |    ^       ^
;    |    |       |
;    +----|-> <lambda>
;         |   parameters: x
;         |   body: (let ((...
;         |
;         +------------------------------------+
;     [1] |                                [2] |
;     +--------------------------------+   +--------------------------------+
;     | x: 3                           |   | x: 2                           |
;     | previously-computed-result: #f |   | previously-computed-result: #f |
;     +--------------------------------+   +--------------------------------+
;
; When (f x) completes, the result is written in the table and the function
; returns it. This is how the environment looks after (make-fib 2) has
; returned:
;
; globals:
; +-------------------------------------------------------------------------+
; | memoize: <procedure>                                                    |
; | memo-fib: <procedure>                                                   |
; +--|----------------------------------------------------------------------+
;    |            ^
;    |            |
;    |   +----------------+
;    |   | f: <procedure> --------> <lambda>
;    |   +----------------+         params: n
;    |            ^                 body: (cond ((= n 0) 0)
;    |            |                             ...
;    |   +----------------+
;    |   | table: <table> --------> (*table (2 . 2)
;    |   +----------------+                 (1 . 1)
;    |    ^       ^                         (0 . 0))
;    |    |       |
;    +----|-> <lambda>
;         |   parameters: x
;         |   body: (let ((...
;     [1] |
;     +--------------------------------+
;     | x: 3                           |
;     | previously-computed-result: #f |
;     +--------------------------------+
;
; Now the second part of [1] computes, which is (memo-fib 1). This time it is
; found in the table and instead of calling f, the lookup just returns 1. The
; addition is carried out and the final result, 3, is written in the table and
; then returned. In the end, we have the following environment:
;
; globals:
; +-------------------------------------------------------------------------+
; | memoize: <procedure>                                                    |
; | memo-fib: <procedure>                                                   |
; +--|----------------------------------------------------------------------+
;    |            ^
;    |            |
;    |   +----------------+
;    |   | f: <procedure> --------> <lambda>
;    |   +----------------+         params: n
;    |            ^                 body: (cond ((= n 0) 0)
;    |            |                             ...
;    |   +----------------+
;    |   | table: <table> --------> (*table (3 . 3)
;    |   +----------------+                 (2 . 2)
;    |            ^                         (1 . 1)
;    |            |                         (0 . 0))
;    +------> <lambda>
;             parameters: x
;             body: (let ((...
;
; You can note, that (memo-fib 1) invoked f only once. Even if it had to be
; calculated once in [1] and once in [2]. Furthermore, note that if we call
; (memo-fib 3) now, f would not get invoked at all. That's why the steps are
; proportional to n.
;
; This scheme would not work if we did:
;
;   (define memo-fib (memoize fib))
;
; Or more accuratelly, it would work half-way. The problem is that fib calls
; recursively itself, instead of memo-fib. Thus, intermediate values are not
; calculated and the time is still exponential. The only benefit is that if we
; call memo-fib with the same argument twice, it will reuse the result from
; the first calculation.
