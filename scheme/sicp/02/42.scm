; SICP exercise 2.42
;
; The "eight-queens puzzle" asks how to place eight queens on a chessboard so
; that no queen is in check from any other (i.e., no two queens are in the same
; row, column, or diagonal). One possible solution is shown in Figure 2.8. One
; way to solve the puzzle is to work accross the board, placing a queen in each
; column. Once we have placed k - 1 queens, we must place the kth queen in a
; position where it does not check any of the queens already on the board. We
; can formulate this approach recursively: Assume that we have already
; generated the sequence of all possible ways to place k - 1 queens in the
; first k - 1 columns of the board. For each of these ways, generate an
; extended set of positions by placing a queen in each row of the kth column.
; Now filter these, keeping only the positions for which the queen in the kth
; column is safe with respect to other queens. This produces the sequence of
; all ways to place k queens in the first k columns. By continuation of this
; process, we will produce not only one solution, but all solutions to the
; puzzle.
;
; We implement this solution as a procedure queens, which returns a sequence
; of all solutions to the problem of placing n queens on n ╳ n chessboard.
; queens has an internal procedure queen-cols that returns the sequence of all
; ways to place queens in the first k columns of the board.
;
; (define (queens board-size)
;   (define (queen-cols k)
;     (if (= k 0)
;         (list empty-board)
;         (filter
;           (lambda (positions) (safe? k positions))
;           (flatmap
;             (lambda (rest-of-queens)
;               (map (lambda (new-row)
;                      (adjoin-position new-row
;                                       k
;                                       rest-of-queens))
;                    (enumerate-interval 1 board-size)))
;             (queen-cols (- k 1))))))
;   (queen-cols board-size))
;
; In this procedure rest-of-queens is a way to place k - 1 queens in the first
; k - 1 columns, and new-row is a proposed row in which to place the queen for
; the kth column. Complete the program by implementing the representation for
; sets of board positions, including the procedure adjoin-position, which
; adjoins a new row-column position to a set of positions, and empty-board,
; which represents an empty set of positions. You must also write the procedure
; safe?, which determines for a set of positions whether the queen in the kth
; column is safe with respect to the others. (Note that we need only check
; whether the new queen is safe - the other queens are already guaranteed safe
; with respet to each other).

(define (queens board-size)
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
