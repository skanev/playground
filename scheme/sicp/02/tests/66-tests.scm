(require rackunit rackunit/text-ui)
(load "../66.scm")

(define a-tree
  '((99 a) ((50 b) ((25 c) ((12 d) () ())
                           ((42 e) ((30 f) () ())
                                   ()))
                   ((75 g) () ()))
           ()))

(define (name-for number)
  (let ((record (lookup number a-tree)))
    (if record
        (name record)
        #f)))

(define sicp-2.66-tests
  (test-suite
    "Tests for SICP exercise 2.66"

    (check-equal? (name-for 99) 'a)
    (check-equal? (name-for 50) 'b)
    (check-equal? (name-for 25) 'c)
    (check-equal? (name-for 12) 'd)
    (check-equal? (name-for 42) 'e)
    (check-equal? (name-for 30) 'f)
    (check-equal? (name-for 75) 'g)

    (check-equal? (name-for 20) #f)
))

(run-tests sicp-2.66-tests)
