; SICP exercise 5.21
;
; Implement register machines for the following procedures. Assume that the
; list-structure memory operations are available as machine primitives.
;
; a. Recursive count-leaves
;
; (define (count-leaves tree)
;   (cond ((null? tree) 0)
;         ((not (pair? tree)) 1)
;         (else (+ (count-leaves (car tree))
;                  (count-leaves (cdr tree))))))
;
; b. Recursive count-leaves with explicit counter
;
; (define (count-leaves tree)
;   (define (count-iter tree n)
;     (cond ((null? tree) n)
;           ((not (pair? tree)) (+ n 1))
;           (else (count-iter (cdr tree)
;                             (count-iter (car tree) n)))))
;   (count-iter tree 0))

; I have defined the necessary-to-run code in helpers/memory.scm.
;
; a. Recursive count-leaves

(define count-leaves-machine
  (make-machine-with-memory '(result n tree continue)
                            '(
                                (assign continue (label count-done))
                              count-leaves
                                (test (op null?) (reg tree))
                                (branch (label tree-null))
                                (test (op pair?) (reg tree))
                                (branch (label tree-pair))
                                (goto (label tree-number))
                              tree-null
                                (assign result (const (n 0)))
                                (goto (reg continue))
                              tree-number
                                (assign result (const (n 1)))
                                (goto (reg continue))
                              tree-pair
                                (save continue)
                                (save tree)
                                (assign tree (op vector-ref) (reg the-cars) (reg tree))
                                (assign continue (label tree-pair-after-car))
                                (goto (label count-leaves))
                              tree-pair-after-car
                                (restore tree)
                                (assign n (reg result))
                                (save n)
                                (assign tree (op vector-ref) (reg the-cdrs) (reg tree))
                                (assign continue (label tree-pair-after-cdr))
                                (goto (label count-leaves))
                              tree-pair-after-cdr
                                (restore n)
                                (restore continue)
                                (assign result (op +) (reg result) (reg n))
                                (goto (reg continue))
                              count-done)))

; b. Recursive count-leaves with explicit counter

(define count-leaves-explicit-counter-machine
  (make-machine-with-memory '(result n tree continue)
                            '(
                                (assign n (const (n 0)))
                                (assign continue (label count-done))
                              count-iter
                                (test (op null?) (reg tree))
                                (branch (label null-tree))
                                (test (op pair?) (reg tree))
                                (branch (label pair-tree))
                                (goto (label number-tree))
                              null-tree
                                (assign result (reg n))
                                (goto (reg continue))
                              number-tree
                                (assign result (op +) (reg n) (const (n 1)))
                                (goto (reg continue))
                              pair-tree
                                (save tree)
                                (save continue)
                                (assign tree (op vector-ref) (reg the-cars) (reg tree))
                                (assign continue (label after-car))
                                (goto (label count-iter))
                              after-car
                                (restore continue)
                                (restore tree)
                                (assign n (reg result))
                                (assign tree (op vector-ref) (reg the-cdrs) (reg tree))
                                (goto (label count-iter))
                              count-done)))
