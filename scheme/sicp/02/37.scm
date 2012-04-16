; SICP exercise 2.37
;
; Suppose we represent vectors v = (vᵢⱼ) as sequences of numbers, and matrices
; m = (mᵢⱼ) as sequences of vectors (rows of the matrix). For example, the
; matrix
;
;   1 2 3 4
;   4 5 6 6
;   6 7 8 9
;
; is represented as the sequence ((1 2 3 4) (4 5 6 6) (6 7 8 9)). With this
; representation, we can use sequence operations to concisely express the basic
; matrix and vector operations. These operations (which are described in any
; book on matrix algebra) are the following:
;
;     (dot-product v w) returns the sum Σᵢvᵢwᵢ
; (matrix-*-vector m v) returns the vector t, where tᵢ = Σⱼmᵢⱼvⱼ
; (matrix-*-matrix m n) returns the matrix p, where pᵢⱼ = Σᵤmᵢᵤvᵤⱼ
;         (transpose m) returns the matrix n, where nᵢⱼ = mⱼᵢ
;
; We can define the dot product as
;
; (define (dot-product v w)
;   (accumulate + 0 (map * v w)))
;
; Fill in the missing expressions in the following procedures for computing the
; other matrix operations. (The procedure accumulate-n is defined exercise 2.36.)
;
; (define (matrix-*-vector m v)
;   (map <??> m))
;
; (define (transpose mat)
;   (accumulate-n <??> <??> mat))
;
; (define (matrix-*-matrix m n)
;   (let ((cols (transpose n)))
;     (map <??> m)))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))



(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define nil '())
