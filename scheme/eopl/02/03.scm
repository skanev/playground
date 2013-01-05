; EOPL exercise 2.03
;
; Define a representation of all the integers (negative and nonnegative) as
; diff-trees, where a diff-tree is a list defined by the grammar
;
;   Diff-tree ::= (one) | (diff Diff-tree Diff-tree)
;
; The list (one) represents 1. If t₁ represents n₁ and t₂ represents n₂, then
; (diff t₁ t₂) is a representation of n₁ - n₂.
;
; So both (one) and (diff (one) (diff (one) (one))) are representations of 1;
; (diff (diff (one) (one)) (one)) is a representation of -1.
;
; 1. Show that every number has infinitely many representations in this
;    system.
; 2. Turn this representation of the integers into an implementation by
;    writing zero, is-zero?, successor and predecessor, as specified on
;    page 32, except that now the negative integers are also represented. Your
;    procedures should take as input any of the multiple legal representations
;    of an integer in this scheme. For example, if your successor procedure is
;    given any of the infinitely many legal representations of 1, it should
;    produce one of the legal representations of 2. It is permissible for
;    different legal representations of 1 to yield different representations
;    of 2.
; 3. Write a proedure diff-tree-plus that does addition in this
;    representation. Your procedure should be optimized for the diff-tree
;    representation, and should do its work in a constant amount of time
;    (independent of the size of its inputs). In particular, it should not be
;    recursive.

; 1. It's rather obvious that a number has infinitelly many representations.
;
; Anyway, if n is represented as (diff M S), then we can also represent it as
; (diff (diff M (one)) (diff S (one))). It will be the same number. We can
; apply this infinitely many times. This is not the only way to modify the
; representation of the number, but it is the simplest.
;
; 2. Let's take a nice layered approach.
;
; First, here are constructors for the representation:

(define (one) '(one))
(define (diff left right) `(diff ,left ,right))

; Here are some observers:

(define (one? diff-tree) (eqv? (car diff-tree) 'one))
(define (diff? diff-tree) (eqv? (car diff-tree) 'diff))
(define (diff-first diff-tree) (cadr diff-tree))
(define (diff-second diff-tree) (caddr diff-tree))

; Here are a few higher-level observers minuend and subtrahend tread (one)
; as (diff (one) (diff (one) (one))).

(define (minuend diff-tree)
  (if (one? diff-tree)
      (one)
      (diff-first diff-tree)))
(define (subtrahend diff-tree)
  (if (one? diff-tree)
      (diff (one) (one))
      (diff-second diff-tree)))

; Here are the four operations we have to implement. Note that is-zero?
; explicitly converts the diff-tree to an integer and compares it with 0.
; Since we know how successor and predecessor work, there is probably a more
; interesting way to check (without conversion), but I don't care enough to
; figure it out.

(define (zero)
  (diff (one) (one)))

(define (is-zero? n)
  (define (to-int n)
    (if (one? n)
        1
        (- (to-int (minuend n))
           (to-int (subtrahend n)))))
  (zero? (to-int n)))

(define (successor n)
  (diff (minuend n)
        (diff (subtrahend n) (one))))

(define (predecessor n)
  (diff n (one)))

; 3. diff-tree-plus

(define (diff-tree-plus diff-tree-1 diff-tree-2)
  (diff diff-tree-1
        (diff (subtrahend diff-tree-2)
              (minuend diff-tree-2))))
