; SICP exercise 3.09
;
; In section 1.2.1 we used the substitution model to analyze two procedures
; for computing factorials, a recursive version
;
;   (define (factorial n)
;     (if (= n 1)
;         1
;         (* n (factorial (- n 1)))))
;
; and an iterative version
;
;   (define (factorial n)
;     (fact-iter 1 1 n))
;
;   (define (fact-iter product counter max-count)
;     (if (> counter max-count)
;         product
;         (fact-iter (* counter product)
;                    (+ counter 1)
;                    max-count)))
;
; Show the environment structures created by evaluating (factorial 6) using
; each version of the factorial procedure.

; With the first version, we have the following environments on each call:
;
;   +------+   +------+   +------+   +------+   +------+   +------+
;   | n: 6 |   | n: 5 |   | n: 4 |   | n: 3 |   | n: 2 |   | n: 1 |
;   +------+   +------+   +------+   +------+   +------+   +------+
;
; With the second version, we have the following environments:
;
;   +---------------------------------------------------------------------+
;   | factorial: <function>                                               |
;   | fact-iter: <function>                                               |
;   +---------------------------------------------------------------------+
;           ^                  ^                    ^           ^     ^
;           |                  |                    |           |     |
;   +--------------+   +---------------+   +----------------+   |     |
;   |   product: 1 |   |   product:  1 |   |   product:   2 |   |   +------+
;   |   counter: 1 |   |   counter:  2 |   |   counter:   3 |   |   | n: 6 |
;   | max-count: 6 |   | max-count:  6 |   | max-count:   6 |   |   +------+
;   +--------------+   +---------------+   +----------------+   |   (factorial)
;                                                               |
;           +------------------+--------------------+-----------+--------+
;           |                  |                    |                    |
;   +--------------+   +---------------+   +----------------+   +----------------+
;   |   product: 6 |   |   product: 24 |   |   product: 120 |   |   product: 720 |
;   |   counter: 4 |   |   counter:  5 |   |   counter:   6 |   |   counter:   7 |
;   | max-count: 6 |   | max-count:  6 |   | max-count:   6 |   | max-count:   6 |
;   +--------------+   +---------------+   +----------------+   +----------------+
