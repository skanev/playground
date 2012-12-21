(require rackunit rackunit/text-ui)
(load "helpers/simulator.scm")
(load "../16.scm")

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

(define sicp-5.16-tests
  (test-suite
    "Tests for SICP exercise 5.16"

    (test-begin "Tracing instructions"
      (reset-tracing!)
      (machine 'trace-on)
      ((machine 'install-trace-proc) collect-instructions)
      (start machine)
      (check-equal? (get-instructions)
                    '((goto (label middle))
                      (assign a (const 2))
                      (assign a (const 3)))))

    (test-begin "Tracing turned off"
      (reset-tracing!)
      (machine 'trace-off)
      ((machine 'install-trace-proc) collect-instructions)
      (start machine)
      (check-equal? (get-instructions) '()))
))

(run-tests sicp-5.16-tests)
