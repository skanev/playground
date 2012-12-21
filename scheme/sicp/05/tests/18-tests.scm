(require rackunit rackunit/text-ui)
(load "helpers/simulator.scm")
(load "../18.scm")

(define traces '())
(define (get-traces)
  (reverse traces))
(define (collect-traces name old new)
  (set! traces (cons `(,name old = ,old new = ,new) traces)))
(define (reset-tracing!)
  (set! traces '()))

(define machine
  (make-machine '(a) '()
                '((assign a (const 1))
                  (assign a (const 2)))))

(define sicp-5.18-tests
  (test-suite
    "Tests for SICP exercise 5.18"

    (test-case "Tracing a register"
      (reset-tracing!)
      ((machine 'install-register-trace-proc) collect-traces)
      ((machine 'register-trace-on) 'a)

      (start machine)

      (check-equal? (get-traces)
                    '((a old = *unassigned* new = 1)
                      (a old = 1            new = 2))))

    (test-case "Tracing turned off"
      (reset-tracing!)
      ((machine 'install-register-trace-proc) collect-traces)
      ((machine 'register-trace-off) 'a)

      (start machine)

      (check-equal? (get-traces) '()))
))

(run-tests sicp-5.18-tests)
