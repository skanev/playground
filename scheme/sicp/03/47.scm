; SICP exercise 3.47
;
; A semaphore (of size n) is a generalization of a mutex. Like a mutex, a
; semaphore supports acquire and release operations, but it is more general in
; that up to n processes can acquire it concurrently. Additional processes
; that attempt to acquire the semaphore must wait for release operations. Give
; implementations of semaphores
;
; a. in terms of mutexes
; b. in terms of atomic test-and-set! operations.

; a.

(define (make-semaphore n)
  (let ((lock (make-mutex)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (lock 'acquire)
             (cond ((> n 0)
                    (set! n (- n 1))
                    (lock 'release))
                   (else
                    (lock 'release)
                    (the-semaphore 'acquire))))
            ((eq? m 'release)
             (lock 'acquire)
             (set! n (+ n 1))
             (lock 'release))))
    the-semaphore))

; b. This sucks. Oh well.

(define (make-semaphore n)
  (let ((lock (list false)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (if (or (= n 0) (test-and-set! lock))
                 (the-semaphore 'acquire)
                 (begin (set! n (- n 1))
                        (clear! lock))))
            ((eq? m 'release)
             (if (test-and-set! lock)
                 (the-semaphore 'release)
                 (begin (set! n (+ n 1))
                        (clear! lock))))))
    the-semaphore))
