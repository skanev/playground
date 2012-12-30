(require rackunit rackunit/text-ui)
(load "../52.scm")

(define sicp-5.52-tests
  (test-suite
    "Tests for SICP exercise 5.52"

    (write-interpreter interpreter-in-c)
    (compile-runtime)
    (check-equal? (interpreter-test-results)
                  "(passed = 27 failed = 0)")
))

(run-tests sicp-5.52-tests)
