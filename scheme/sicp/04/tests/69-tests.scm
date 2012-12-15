(require rackunit rackunit/text-ui)
(load "../69.scm")

(define (matches-of query)
  (let ((processed-query (query-syntax-process query)))
    (stream->list
      (stream-map
        (lambda (frame)
          (instantiate-exp processed-query frame (lambda (v f) (contract-question-mark v))))
        (qeval processed-query (singleton-stream '()) '())))))


(define sicp-4.69-tests
  (test-suite
    "Tests for SICP exercise 4.69"

    (check-equal? (matches-of '((great grandson) ?g ?ggs))
                  '(((great grandson) Adam Irad)
                    ((great grandson) Cain Mehujael)
                    ((great grandson) Enoch Methushael)
                    ((great grandson) Irad Lamech)
                    ((great grandson) Mehujael Jabal)
                    ((great grandson) Mehujael Jubal)))

    (check-equal? (matches-of '(?relationship Adam Irad))
                  '(((great grandson) Adam Irad)))
))

(run-tests sicp-4.69-tests)
