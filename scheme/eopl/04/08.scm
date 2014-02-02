; EOPL exercise 4.08
;
; Show exactly where in our implementation of the store these operations take
; linear time rather than constant time.

; In newref, calculating the next reference takes linear time. Furthermore,
; appending a new element to a list is also linear, since append needs to copy
; the list.

(define (newref val)
  (let ((next-ref (length the-store)))             ; length is linear to the-store
    (set! the-store (append the-store (list val))) ; append is linear to the-store
    next-ref))

; When dereferencing, scanning the list with list-ref is also linear

(define (deref ref)
  (list-ref the-store ref))

; Finally, setref! needs to construct a new list with one element changed.
; Since the element can be the last element in the array, this operation is
; also linear.

(define (setref! ref val)
  (set! the-store
    (let recur ((store1 the-store)
                (ref1 ref))
      (cond ((null? store1)
             (eopl:error 'setref! "Invalid reference ~s in ~s" ref the-store))
            ((zero? ref1)
             (cons val (cdr store1)))
            (else (cons (car store1)
                        (recur (cdr store1) (- ref1 1))))))))
