; SICP exercise 2.43
;
; Louis Reasoner is having a terrible time doing Exercise 2.42. His queens
; procedure seems to work, but it runs extremely slow. (Louis never does manage
; to wait long enough for it to solve even the 6x6 case.) When Louis asks Eva
; Lu Ator for help, she points out that he has interchanged the order of the
; nested mappings in the flatmap, writing it as
;
; (flatmap
;   (lambda (new-row)
;     (map (lambda (rest-of-queens)
;            (adjoin-position new-row k rest-of-queens))
;          (queen-cols (- k 1))))
;   (enumerate-interval 1 board-size))
;
; Explain why this interchange makes the program run slowly. Estimate how long
; it will take Louis's program to solve the eight-queens puzzle, assuming that
; the program in Exercise 2.42 solves the puzzle in time T.

; It appears Mr. Reasoner has a lot to learn - he keeps fumbling. Anyhow:
;
; In 2.42 we generated a valid nxn board with (queen-cols) and then extended it
; with n + 1 queens. In Louis's program, when generating a nxn board, we
; calculate the (n-1)x(n-1) boards n times. This means, that we end up doing
; the full calculation 1 time for the 2x2 board, 2 times for the 3x3 board, 3
; times for the 4x4 board and so forth. In the end, we end up doing it 7!
; times. So if the program in 2.42 solves the puzzle in time T, Louis's would
; do it in 7!T. Pretty bad.
;
; Let's see what the numbers tell us.
;
; Slow queens: 24892.343017578125
; Fast queens: 5.5859375
;
; The result I expected is ~28152 milliseconds, which is 13% off. Oh well.
; At least I can wait for it :)

(define (slow-queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
            (flatmap
              (lambda (new-row)
                (map (lambda (rest-of-queens)
                       (adjoin-position new-row k rest-of-queens))
                     (queen-cols (- k 1))))
              (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

(define (fast-queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                     (adjoin-position new-row
                                      k
                                      rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
  (queen-cols board-size))



(define empty-board '())

(define (adjoin-position new-row k rest-of-queens)
  (cons (list new-row k) rest-of-queens))

(define (safe? k positions)
  (define queen-position (queen-at k positions))
  (let ((q1r (car queen-position))
        (q1c (cadr queen-position)))
    (all?
      (lambda (position)
        (let ((q2r (car position))
              (q2c (cadr position)))
          (or (and (= q1r q2r)
                   (= q1c q2c))
              (and (not (= q1r q2r))
                   (not (= q1c q2c))
                   (not (= (+ q1r q1c)
                           (+ q2r q2c)))
                   (not (= (- q1r q1c)
                           (- q2r q2c)))))))
      positions)))



(define (queen-at column positions)
  (if (= column (cadar positions))
      (car positions)
      (queen-at column (cdr positions))))

(define (all? proc seq)
  (cond ((null? seq) #t)
        ((proc (car seq)) (all? proc (cdr seq)))
        (else #f)))



(define (enumerate-interval a b)
  (if (> a b)
    (list)
    (cons a
          (enumerate-interval (+ a 1) b))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))



(define (time message proc)
  (let ((start (current-inexact-milliseconds)))
    (proc)
    (let ((time-taken (- (current-inexact-milliseconds) start)))
      (printf "~a: ~a\n" message time-taken))))



; Warm up
(slow-queens 2)
(fast-queens 2)

; Timing
(time "Slow queens" (lambda () (slow-queens 8)))
(time "Fast queens" (lambda () (fast-queens 8)))
