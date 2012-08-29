; SICP exercise 3.23
;
; A deque ("double-ended queue") is a sequence in which items can be
; inserted and deleted at either the front or the rear. Operations on deques
; are the constructor make-deque, the predicate empty-deque?, selectors
; front-deque and rear-deque and mutators front-insert-deque!,
; rear-insert-deque!, front-delete-deque! and rear-delete-deque!. Show how to
; represent deques using pairs, and give implementation of the operations. All
; operations should be accomplished in Θ(1) steps.

; Oooh, nice! This requires implementing a doubly-linked list. The segments of
; the lists will be created with make-segment and each will contain an item
; and a pointer to the previous and the next segment in the queue.

(require r5rs/init)

(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))

(define (make-segment item previous next) (cons item (cons previous next)))
(define (item segment) (car segment))
(define (previous segment) (cadr segment))
(define (next segment) (cddr segment))
(define (set-previous! segment previous) (set-car! (cdr segment) previous))
(define (set-next! segment next) (set-cdr! (cdr segment) next))

(define (make-deque) (cons '() '()))
(define (empty-deque? deque) (null? (front-ptr deque)))
(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (item (front-ptr deque))))
(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (item (rear-ptr deque))))
(define (front-insert-deque! deque item)
  (let* ((front (front-ptr deque))
         (new-segment (make-segment item '() front)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-segment)
           (set-rear-ptr! deque new-segment))
          (else
           (set-previous! front new-segment)
           (set-front-ptr! deque new-segment)))
    deque))
(define (rear-insert-deque! deque item)
  (let* ((rear (rear-ptr deque))
         (new-segment (make-segment item rear '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-segment)
           (set-rear-ptr! deque new-segment))
          (else
           (set-next! rear new-segment)
           (set-rear-ptr! deque new-segment)))
    deque))
(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "FRONT-DELETE called with an empty deque" deque))
        ((null? (next (front-ptr deque)))
         (set-front-ptr! deque '())
         (set-rear-ptr! deque '()))
        (else
         (set-front-ptr! deque (next (front-ptr deque)))
         (set-previous! (front-ptr deque) '())))
  deque)
(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "REAR-DELETE called with an empty deque" deque))
        ((null? (previous (rear-ptr deque)))
         (set-front-ptr! deque '())
         (set-rear-ptr! deque '()))
        (else
         (set-rear-ptr! deque (previous (rear-ptr deque)))
         (set-next! (rear-ptr deque) '())))
  deque)
