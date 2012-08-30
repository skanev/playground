; SICP exercise 3.32
;
; The procedures to be run during each time segment of the agenda are kept in
; a queue. Thus, the procedures for each segment are called in the order in
; which they were added to the agenda (first-in, first-out). Explain why this
; order must be used. In particular, trace the behavior of an and-gate whose
; inputs change from 0, 1 to 1, 0 in the same segment and say how the behavior
; would differ if we stored a segment's procedures in an ordinary list, adding
; and removing procedures only at the front (last in, first out).

; Each action calculates the new result to be set before it is added to the
; queue. If we change 0, 1 to 1, 0, we will add two actions to the queue:
;
;   a1: set output to 1
;   a2: set output to 0
;
; The reason that we set those outputs is because the gate reads the wire
; signals before it puts the action after a specific delay. When we change
; from (0, 1) to (1, 1) it will determine that the output should be 1. We
; immediatelly change (1, 1) to (1, 0) and the action will set the output to
; 0.
;
; That is, assuming, we use a queue. If we use a stack (last in, first out),
; those actions will get executed in reverse order. That is, a2 will set the
; output to 0, after which a1 will set it to 1. In the end, the and gate will
; output erroneous result.
;
; You can run this file to verify.

(require r5rs/init)

; Primitive function boxes

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

; Logical functions

(define (logical-and a b)
  (cond ((and (= a 0) (= b 0)) 0)
        ((and (= a 0) (= b 1)) 0)
        ((and (= a 1) (= b 0)) 0)
        ((and (= a 1) (= b 1)) 1)
        (else (error "Invalid signals" a b))))

; Wires

(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures)))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (get-signal wire) (wire 'get-signal))
(define (set-signal! wire new-value) ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure) ((wire 'add-action!) action-procedure))

; The agenda

(define (make-time-segment time actions) (cons time actions))
(define (segment-time s) (car s))
(define (segment-actions s) (cdr s))
(define (set-segment-actions! s actions) (set-cdr! s actions))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time) (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments) (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))
(define (empty-agenda? agenda) (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (make-time-segment time (list action)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (set-segment-actions! (car segments)
                              (cons action (segment-actions (car segments))))
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr! segments (cons (make-new-time-segment time action) rest))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments! agenda (cons (make-new-time-segment time action) segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (set-segment-actions! (first-segment agenda)
                        (cdr (segment-actions (first-segment agenda))))
  (if (null? (segment-actions (first-segment agenda)))
      (set-segments! agenda (rest-segments agenda))
      'done))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (car (segment-actions first-seg)))))

; Delays

(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

; Simulation infrastructure

(define the-agenda (make-agenda))
(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display "  New-value = ")
                 (display (get-signal wire))
                 (newline))))

(define input-1 (make-wire))
(define input-2 (make-wire))
(define output (make-wire))
(define carry (make-wire))

(probe 'output output)

(and-gate input-1 input-2 output)

(set-signal! input-1 0)
(set-signal! input-2 1)
(propagate)

(set-signal! input-1 1)
(set-signal! input-2 0)
(propagate)
