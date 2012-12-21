(require rackunit rackunit/text-ui)
(load "helpers/simulator.scm")
(load "../17.scm")

(define instructions '())
(define (get-instructions)
  (reverse instructions))
(define (collect-instructions inst)
  (set! instructions (cons inst instructions)))
(define (reset-tracing!)
  (set! instructions '()))

(define machine
  (make-machine '(a) '()
                '(begin
                    (goto (label middle))
                    (assign a (const 1))
                  middle
                    (assign a (const 2))
                    (assign a (const 3))
                  end)))

(define sicp-5.17-tests
  (test-suite
    "Tests for SICP exercise 5.17"

    (test-begin "Tracing labels"
      (reset-tracing!)
      (machine 'trace-on)
      ((machine 'install-trace-proc) collect-instructions)

      (start machine)

      (check-eq? (machine 'instruction-count) 3)
      (check-equal? (get-instructions)
                    '(begin
                      (goto (label middle))
                      middle
                      (assign a (const 2))
                      (assign a (const 3)))))
))

(run-tests sicp-5.17-tests)
