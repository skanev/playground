(require rackunit rackunit/text-ui)
(load "../69.scm")

(define sicp-3.69-tests
  (test-suite
    "Tests for SICP exercise 3.69"

    (check-equal? (stream-take pythagorean-triples 5)
                  '((3 4 5) (6 8 10) (5 12 13) (9 12 15) (8 15 17)))
))

(run-tests sicp-3.69-tests)
