(require rackunit rackunit/text-ui)
(load "../84.scm")

(define sicp-2.84-tests
  (test-suite
    "Tests for SICP exercise 2.84"

    (check-equal? (supertype 'integer) 'rational)
    (check-equal? (supertype 'rational) 'real)
    (check-equal? (supertype 'real) 'complex)
    (check-equal? (supertype 'complex) #f)

    (check-true (supertype? 'rational 'integer))
    (check-true (supertype? 'real 'integer))
    (check-true (supertype? 'complex 'integer))
    (check-true (supertype? 'real 'rational))
    (check-true (supertype? 'complex 'rational))
    (check-true (supertype? 'complex 'real))
    (check-false (supertype? 'integer 'rational))
    (check-false (supertype? 'integer 'real))
    (check-false (supertype? 'integer 'complex))
    (check-false (supertype? 'rational 'real))
    (check-false (supertype? 'rational 'complex))
    (check-false (supertype? 'real 'complex))

    (check-equal? (foo (make-integer 1) (make-integer 1)) 'foo-integer)
    (check-equal? (foo (make-rational 1 2) (make-integer 1)) 'foo-rational)
    (check-equal? (foo (make-integer 1) (make-rational 1 2)) 'foo-rational)
    (check-equal? (foo (make-integer 1) (make-real 1.0)) 'foo-real)
    (check-equal? (foo (make-rational 1 2) (make-real 1.0)) 'foo-real)
    (check-equal? (foo (make-real 1.0) (make-integer 1)) 'foo-real)
    (check-equal? (foo (make-real 1.0) (make-rational 1 2)) 'foo-real)
    (check-equal? (foo (make-integer 1) (make-complex 1 2)) 'foo-complex)
    (check-equal? (foo (make-rational 1 2) (make-complex 1 2)) 'foo-complex)
    (check-equal? (foo (make-real 1.0) (make-complex 1 2)) 'foo-complex)
    (check-equal? (foo (make-complex 1 2) (make-integer 1)) 'foo-complex)
    (check-equal? (foo (make-complex 1 2) (make-rational 1 2)) 'foo-complex)
    (check-equal? (foo (make-complex 1 2) (make-real 1.0)) 'foo-complex)
))

(run-tests sicp-2.84-tests)
