(require rackunit rackunit/text-ui)
(load "../74.scm")

(define paul
  '((salary 2000)
   (address "Arrakeen Palace")))
(define leto
  '((salary 2500)
    (address "The Caladan planet")))
(define atreides
  (attach-tag 'atreides-file
    (list
      (list "Paul Atreides" paul)
      (list "Duke Leto" leto))))

(define stilgar
  '((income . 1000)
    (location . "Sietch Tabr")))
(define chani
  '((income . 800)
    (location . "Whenever Paul is")))
(define fremen
  (attach-tag 'fremen-file
    (list
      (cons "Stilgar" stilgar)
      (cons "Chani" chani))))

(define sicp-2.74-tests
  (test-suite
    "Tests for SICP exercise 2.74"

    (test-suite "Data structures"
      (check-equal? (a-list-get '(("a" 1) ("b" 2)) "b")
                    2)
      (check-equal? (a-list-get '() 'a) '())

      (check-equal? (p-list-get '((a . 1) (b . 2)) 'b) 2)
      (check-equal? (p-list-get '() 'a) '())
    )

    (test-suite "Tags"
      (check-equal? (attach-tag 'number 1) '(number . 1))
      (check-equal? (type-tag (attach-tag 'number 1)) 'number)
      (check-exn exn? (lambda () (type-tag 1)))
      (check-equal? (contents (attach-tag 'number 1)) 1)
      (check-exn exn? (lambda () (contents 1)))
    )

    (test-suite "get-record"
      (check-equal? (get-record "Paul Atreides" atreides) (attach-tag 'atreides paul))
      (check-equal? (get-record "Duke Leto" atreides) (attach-tag 'atreides leto))
      (check-equal? (get-record "Stilgar" fremen) (attach-tag 'fremen stilgar))
      (check-equal? (get-record "Chani" fremen) (attach-tag 'fremen chani))
      (check-equal? (get-record "Vladimir Harkonnen" atreides) '())
      (check-equal? (get-record "Feyd-Rautha Harkonnen" fremen) '())
    )

    (test-suite "get-salary"
      (check-equal? (get-salary (get-record "Paul Atreides" atreides)) 2000)
      (check-equal? (get-salary (get-record "Duke Leto" atreides)) 2500)
      (check-equal? (get-salary (get-record "Stilgar" fremen)) 1000)
      (check-equal? (get-salary (get-record "Chani" fremen)) 800)
    )

    (test-suite "find-employee-record"
      (check-equal? (find-employee-record "Paul Atreides" (list atreides fremen)) (attach-tag 'atreides paul))
      (check-equal? (find-employee-record "Duke Leto" (list atreides fremen)) (attach-tag 'atreides leto))
      (check-equal? (find-employee-record "Stilgar" (list atreides fremen)) (attach-tag 'fremen stilgar))
      (check-equal? (find-employee-record "Chani" (list atreides fremen)) (attach-tag 'fremen chani))
      (check-equal? (find-employee-record "Vladimir Harkonnen" (list atreides fremen)) '())
    )
))

(run-tests sicp-2.74-tests)
