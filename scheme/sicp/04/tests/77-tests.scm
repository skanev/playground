(require rackunit rackunit/text-ui)
(load "../77.scm")
(load-relative "../showcase/query/database.scm")

(define (matches-of query)
  (let ((processed-query (query-syntax-process query)))
    (stream->list
      (stream-map
        (lambda (frame)
          (instantiate-exp processed-query frame (lambda (v f) (contract-question-mark v))))
        (qeval processed-query (singleton-stream empty-frame))))))

(define sicp-4.77-tests
  (test-suite
    "Tests for SICP exercise 4.77"

    (check-equal? (matches-of '(and (lisp-value > ?amount 30000)
                                    (salary ?person ?amount)))
                  '((and (lisp-value > 150000 30000)
                         (salary (Warbucks Oliver) 150000))
                    (and (lisp-value > 60000 30000)
                         (salary (Bitdiddle Ben) 60000))
                    (and (lisp-value > 40000 30000)
                         (salary (Hacker Alyssa P) 40000))
                    (and (lisp-value > 35000 30000)
                         (salary (Fect Cy D) 35000))
                    (and (lisp-value > 75000 30000)
                         (salary (Scrooge Eben) 75000))))

    (check-equal? (matches-of '(and (not (job ?x (computer programmer)))
                                    (supervisor ?x (Bitdiddle Ben))))
                  '((and (not (job (Tweakit Lem E) (computer programmer)))
                         (supervisor (Tweakit Lem E) (Bitdiddle Ben)))))
))

(run-tests sicp-4.77-tests)
