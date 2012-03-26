; SICP exercise 2.29
;
; A binary mobile consists of two branches, a left branch and a right branch.
; Each branch is a rod of certain length, from which hangs either a weight or
; another binary mobile. We can represent a binary mobile using compund data by
; constructing it from two branches (for example, using list):
;
; (define (make-mobile left right)
;   (list left right))
;
; A branch is constructed from a length (which must be a number) together with
; a structure, which may be either a number (representing a simple weight) or
; another mobile:
;
; (define (make-branch length structure)
;   (list length structure))
;
; a. Write the corresponding selectors left-branch and right-branch, which
; return the branches of a mobile, and branch-length and branch-structure,
; which return the components of a branch.
;
; b. Using your selectors, define a procedure total-weight that returns the
; total weight of a mobile.
;
; c. A mobile is said to be balanced if the torque applied by its top-left
; branch is equal to that applied by its top-right branch (that is, if the
; length of the left rod multiplied by the weight hanging from that rod is
; equal to the corresponding product of the right side) and if each of the
; submobiles hanging off its branches is balanced. Design a predicate that
; tests whether a binary mobile is balanced.
;
; d. Suppose we change the representation of mobiles so that the constructors
; are
;
; (define (make-mobile left right)
;   (cons left right))
;
; (define (make-branch length structure)
;   (cons length structure))
;
; How much do you need to change your programs to convert to the new
; representation?

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; a.
;
; Here are the selectors, with a little extra:

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (weight? structure)
  (not (pair? structure)))

; b.
;
; This is total-weight. It depends on an additional selector.

(define (total-weight structure)
  (if (weight? structure)
      structure
      (+ (total-weight (branch-structure (left-branch structure)))
         (total-weight (branch-structure (right-branch structure))))))

; c.
;
; Testing whether a mobile is balanced is a bit messy. It also makes use of
; weight?

(define (balanced? mobile)
  (define (torque branch)
    (* (branch-length branch)
       (total-weight (branch-structure branch))))
  (define (balanced-submobile? branch)
    (or (weight? (branch-structure branch))
        (balanced? (branch-structure branch))))
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (and (= (torque left) (torque right))
         (balanced-submobile? left)
         (balanced-submobile? right))))

; d.
;
; Let's just do it:

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

; This is what has to change

(define (right-branch mobile)
  (cdr mobile))

(define (branch-structure branch)
  (cdr branch))

; That way we accomplish a neat abstraction barrier. If we delete the last four
; definitions, the tests will continue to pass.
