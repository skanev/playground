; SICP exercise 3.22
;
; Instead of representing the queue as a pair of pointes, we can build a queue
; as a procedure with local state. The local state will consist of pointers to
; the beginning and the end of an ordinary list. Thus, the make-queue
; procedure will have the form
;
; (define (make-queue)
;   (let ((front-ptr ...)
;         (rear-ptr ...))
;     <definitions of internal procedures>
;     (define (dispatch m) ...)
;     dispatch))
;
; Complete the definition of make-queue and provide implementations of the
; queue operations using this representation.

(require r5rs/init)

(define (empty-queue? queue) ((queue 'empty?)))
(define (front-queue queue) ((queue 'front)))
(define (insert-queue! queue item) ((queue 'insert) item))
(define (delete-queue! queue) ((queue 'delete)))

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty?)
      (null? front-ptr))
    (define (front)
      (if (empty?)
          (error "FRONT called with an empty queue")
          (car front-ptr)))
    (define (insert item)
      (let ((new-pair (cons item '())))
        (cond ((empty?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair))
              (else
               (set-cdr! rear-ptr new-pair)
               (set! rear-ptr new-pair)))
        dispatch))
    (define (delete)
      (cond ((empty?)
             (error "DELETE called with an empty queue"))
            (else
              (set! front-ptr (cdr front-ptr))
              dispatch)))

    (define (dispatch m)
      (cond ((eq? m 'empty?) empty?)
            ((eq? m 'front) front)
            ((eq? m 'insert) insert)
            ((eq? m 'delete) delete)
            (else (error "Undefined operation - QUEUE" m))))
    dispatch))
