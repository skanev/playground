; SICP exercise 5.22
;
; Exercise 3.12 of section 3.3.1 presented an append procedure that appends
; two lists to form a new list and an append! procedure that splices two lists
; together. Design a register machine to implement each of these procedures.
; Assume that the list-structure memory operations are available as primitive
; operations.

;   (define (append x y)
;     (if (null? x)
;         y
;         (cons (car x) (append (cdr x) y))))

(define append-machine
  (make-machine-with-memory '(x y val result continue)
                            '(
                                (assign continue (label append-done))
                              append
                                (test (op null?) (reg x))
                                (branch (label x-empty))
                                (assign val (op vector-ref) (reg the-cars) (reg x))
                                (perform (op vector-set!) (reg the-cars) (reg free) (reg val))
                                (assign val (reg free))
                                (save val)
                                (assign free (op +) (reg free) (const (p 1)))
                                (save continue)
                                (assign continue (label after-append))
                                (assign x (op vector-ref) (reg the-cdrs) (reg x))
                                (goto (label append))
                              after-append
                                (restore continue)
                                (restore val)
                                (perform (op vector-set!) (reg the-cdrs) (reg val) (reg result))
                                (assign result (reg val))
                                (goto (reg continue))
                              x-empty
                                (assign result (reg y))
                                (goto (reg continue))
                              append-done
                              )))

(define append!-machine
  (make-machine-with-memory '(x y val)
                            '(append!
                                (assign val (op vector-ref) (reg the-cdrs) (reg x))
                                (test (op null?) (reg val))
                                (branch (label last-pair))
                                (assign x (op vector-ref) (reg the-cdrs) (reg x))
                                (goto (label append!))
                              last-pair
                                (perform (op vector-set!) (reg the-cdrs) (reg x) (reg y))
                              )))
