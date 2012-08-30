; SICP exercise 3.30
;
; Figure 3.27 shows a ripple-carry adder formed by stringing together n
; full-adders. This is the simplest form of parallel adder for adding two
; n-bit binary numbers. The inputs A₁, A₂, A₃, ..., Aᵢ and B₁, B₂, B₃, ..., Bᵢ
; are the two binary numbers to be added (each Aᵣ and Bᵣ is a 0 or a 1). The
; circuit generates S₁, S₂, S₃, ..., Sᵢ, the i bits of the sum, and C , the
; carry from the addition. Write a procedure ripple-carry-adder that generates
; that circuit. The procedure should take as arguments three lists of i wires
; each - the Aᵣ, the Bᵣ and the Sᵣ - and also another wire C. The major
; drawback of the ripple-carry adder is the need to wait for the signals to
; propagate. What is the delay needed to obtain the complete output from an
; i-bit ripple-carry adder, expressed in terms of the delays of and-gates,
; or-gates, and inverters?

; The ripple-carry-adder procedure is defined below.
;
; As for the delay:
;
; The half-adder has the following delays for each output:
;
;   s: and + not + and | or + and (whichever is slower)
;   c: and
;
; The full-adder has the following delays:
;
;   sum: and + not + and | or + and
;   c-out: (and + not + and | or + and) + and + or
;
; Finally, the delay of C an n-bit ripple adder is:
;
;   n-ripple: n((and + not + and | or + and) + and + or)
;
; With the delays we have defined, this is either:
;
;   n(3and + or + not)
;
; or
;
;   n(2or + 2and)
;
; whichever is slower. However, we need wait for the last SUM too. We can
; subtract (and + or) from the time (since this is the time that takes the
; signal to travel through the carry from the second half-adder in the last
; adder and then through the and gate) and then we can add the s time for
; another half-adder, which makes the total time:
;
;   n(3and + or + not) + and + not - or
;
; or, again:
;
;   n(2or + 2and)

(require r5rs/init)

; Ripple-carry adder

(define (ripple-carry-adder a b c-in s c-out)
  (define (ripple a b s c)
    (cond ((null? (cdr a))
           (full-adder (car a) (car b) c-in (car s) c))
          (else
           (let ((w (make-wire)))
             (full-adder (car a) (car b) w (car s) c)
             (ripple (cdr a) (cdr b) (cdr s) w)))))

  (ripple (reverse a) (reverse b) (reverse s) c-out))

; Half-adder & adder

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

; Primitive function boxes

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (or-gate o1 o2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal o1) (get-signal o2))))
      (after-delay or-gate-delay (lambda () (set-signal! output new-value)))))
  (add-action! o1 or-action-procedure)
  (add-action! o2 or-action-procedure)
  'ok)

; Logical functions

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (logical-and a b)
  (cond ((and (= a 0) (= b 0)) 0)
        ((and (= a 0) (= b 1)) 0)
        ((and (= a 1) (= b 0)) 0)
        ((and (= a 1) (= b 1)) 1)
        (else (error "Invalid signals" a b))))

(define (logical-or a b)
  (cond ((and (= a 0) (= b 0)) 0)
        ((and (= a 0) (= b 1)) 1)
        ((and (= a 1) (= b 0)) 1)
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
      (set! action-procedures (cons proc action-procedures))
      (proc))
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

; Queues

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (make-queue) (cons '() '()))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair))
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)))
    queue))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

; The agenda

(define (make-time-segment time queue) (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

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
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr! segments (cons (make-new-time-segment time action) rest))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments! agenda (cons (make-new-time-segment time action) segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda))
        'done)))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

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
