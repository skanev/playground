; SICP exercise 4.76
;
; Our implementation of and as a series combination of queries (figure 4.5) is
; elegant, but it is inefficient because in processing the second query of the
; and we must scan the data base for each frame produced by the first query.
; If the data base has N elements, and a typical query produces a number of
; output frames proportional to N (say N/k), then scanning the data base for
; each frame produced by the first query will require N²/k calls to the
; pattern matcher. Another approach would be to process the two clauses of the
; and separately, then look for all pairs of output frames that are
; compatible. If each query produces N/k output frames, then this means that
; we must perform N²/k² compatibility checks -- a factor of k fewer than the
; number of matches required in our current method.
;
; Devise an implementation of and that uses this strategy. You must implement
; a procedure that takes two frames as inputs, checks whether the bindings in
; the frames are compatible, and, if so, produces a frame that merges the two
; sets of bindings. This operation is similar to unification.

; The implementation is below. It has its disadvantages, though. First, the
; following query stops working:
;
; (and (supervisor ?x (Bitdiddle Ben))
;      (not (job ?x (computer programmer))))
;
; The reason is that (not (job ?x (computer programmer))) results to the empty
; stream of frames.
;
; There is another issue, which is illustrated in the outranked-by rule:
;
; (rule (outranked-by ?staff-person ?boss)
;       (or (supervisor ?staff-person ?boss)
;           (and (supervisor ?staff-person ?middle-manager)
;                (outranked-by ?middle-manager ?boss))))
;
; In this case, outranked-by results to an infinte loop, since
;
; (outranked-by ?staff-person ?boss)
;
; calls directly
;
; (outranked-by ?middle-manager ?boss)
;
; and all frames (not just the reduced set of frames from the previous
; conjunct.

(define (merge-frames frame1 frame2)
  (cond ((null? frame1) frame2)
        ((eq? 'failed frame2) 'failed)
        (else
         (let ((var (binding-variable (car frame1)))
               (val (binding-value (car frame1))))
           (let ((extension (extend-if-possible var val frame2)))
             (merge-frames (cdr frame1) extension))))))

(define (conjoin-frame-streams stream1 stream2)
  (stream-flatmap
    (lambda (frame1)
      (stream-filter
        (lambda (frame) (not (eq? frame 'failed)))
        (stream-map
          (lambda (frame2) (merge-frames frame1 frame2))
          stream2)))
    stream1))

(define (faster-conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin-frame-streams
        (qeval (first-conjunct conjuncts) frame-stream)
        (conjoin (rest-conjuncts conjuncts) frame-stream))))

(put 'and 'qeval faster-conjoin)
