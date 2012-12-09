(require rackunit rackunit/text-ui)
(load "../34.scm")

(define (run exp)
  (actual-value exp (setup-environment)))

(define (to-s exp)
  (with-output-to-string (lambda () (run `(begin (define result ,exp)
                                                 (print result))))))

(define sicp-4.34-tests
  (test-suite
    "Tests for SICP exercise 4.34"

    (check-equal? (to-s ''()) "()")
    (check-equal? (to-s ''(() a)) "(() a)")
    (check-equal? (to-s ''(a b c)) "(a b c)")
    (check-equal? (to-s ''(a (b c) d)) "(a (b c) d)")
    (check-equal? (to-s ''(a . b)) "(a . b)")
    (check-equal? (to-s '(begin (define pair (cons 'a pair))
                                pair))
                  "(a (...))")
    (check-equal? (to-s '(begin (define pair (cons 'a (cons pair 'b)))
                                pair))
                  "(a (...) . b)" )
    (check-equal? (to-s '(begin (define pair (cons 'a (cons pair (cons 'b '()))))
                                pair))
                  "(a (...) b)")
))

(run-tests sicp-4.34-tests)
