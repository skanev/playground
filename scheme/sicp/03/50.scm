; SICP exercise 3.50
;
; Complete the following definition, which generalizes stream-map to allow
; procedures that take multiple arguments analogous to map in section 2.2.1,
; footnote 12.
;
;   (define (stream-map proc . argstreams)
;     (if (<??> (car argstreams))
;         the-empty-stream
;         (<??>
;          (apply proc (map <??> argstreams))
;          (apply stream-map
;                 (cons proc (map <??> argstreams))))))

(define the-empty-stream empty-stream)
(define stream-null? stream-empty?)
(define stream-car stream-first)
(define stream-cdr stream-rest)

(define (stream-map2 proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (stream-cons
       (apply proc (map stream-car argstreams))
       (apply stream-map2
              (cons proc (map stream-cdr argstreams))))))
