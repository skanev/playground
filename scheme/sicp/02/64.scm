; SICP exercise 2.64
;
; The following procedure list->tree converts an ordered list to a balanced
; binary tree. The helper procedure partial-tree takes as arguments an integer n
; and a list of at least n elements and constructs a balanced tree containing
; the first n elements of the list. The result returned by partial-tree is a pair
; (formed with cons) whose car is the constructed tree and whose cdr is the list
; of elements not included in the tree.
;
; (define (list->tree elements)
;   (car (partial-tree elements (length elements))))
;
; (define (partial-tree elts n)
;   (if (= n 0)
;       (cons '() elts)
;       (let* ((left-size (quotient (- n 1) 2))
;              (left-result (partial-tree elts left-size))
;              (left-tree (car left-result))
;              (non-left-elts (cdr left-result))
;              (right-size (- n (+ left-size 1)))
;              (this-entry (car non-left-elts))
;              (right-result (partial-tree (cdr non-left-elts) right-size))
;              (right-tree (car right-result))
;              (remaining-elts (cdr right-result)))
;         (cons (make-tree this-entry left-tree right-tree) remaining-elts))))
;
; a. Write a short paragraph explaining as clearly as you can how partial-tree
;    works. Draw the tree produced by list->tree for the list (1 3 5 7 9 11)
; b. What is the order of growth in the number of steps required by list->tree
;    to convert a list of n elements?

; a. The procedure works in a fairly simple fashion.
;
; It splits the list in three parts - a left sub-list, a right sub-list and the
; element between them. The parts are roughly equal in size. The result is a
; tree whose entry is the middle element and whose branches are the sub-lists
; transformed to trees with the same procedure (recursively).
;
; Once the procedure arrives to a list with size <= 3, it is trivial to
; visualize how the tree would look like. Lists of sizes > 3 get reduced to
; those cases with recursion.
;
; The final tree is binary, because the left branch contains elements that are
; smaller than the middle element and the right branch contains only elements
; that are greater than the middle element. It is balanced, because the
; algorithm halves the list size on each step, which means that the maximum
; depth of the tree will be log(n).
;
; FYI, (list->tree '(1 3 5 7 9 11)) produces:
;
;       5
;     /   \
;   1       9
;    \     / \
;     3   7   11
;
; b. Θ(n)
;
; Each list item is visited only once and each visit performs a single cons.
