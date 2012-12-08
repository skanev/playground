; SICP exercise 4.24
;
; Design and carry out some experiments to compare the speed of the original
; metacircular evaluator with the version in this section. Use your results to
; estimate the fraction of time spent in analysis versus execution for various
; procedures.

; This solution steps away from the traditional approach and uses a bit more
; of Racket's functionality (although there is probably a better way to do
; this).
;
; The output, slightly rearranged is:
;
;   evaluator recursive-fib-25: cpu time: 941 real time: 943 gc time: 10
;   analyzing recursive-fib-25: cpu time: 475 real time: 477 gc time: 92
;
;   evaluator odd-even-1000000: cpu time: 221 real time: 221 gc time: 2
;   analyzing odd-even-1000000: cpu time: 116 real time: 118 gc time: 8
;
;   evaluator factorial-100000: cpu time: 98 real time: 99 gc time: 27
;   analyzing factorial-100000: cpu time: 71 real time: 73 gc time: 16
;
; Unsurprisingly, the analyzing evaluator is faster.

(module benchmark racket/load
  (require r5rs/init)

  (define recursive-fib-25
    '(begin (define (fib n)
              (cond ((= n 0) 0)
                    ((= n 1) 1)
                    (else (+ (fib (- n 1)) (fib (- n 2))))))
            (fib 25)))

  (define factorial-100000
    '(begin (define (factorial n)
              (if (= n 0)
                  1
                  (* n (factorial (- n 1)))))
            (factorial 10000)))
  (define odd-even-1000000
    '(begin (define (odd? n)
              (if (= n 0)
                  false
                  (even? (- n 1))))
            (define (even? n)
              (if (= n 0)
                  true
                  (odd? (- n 1))))
            (even? 100000)))

  (define experiments
    (list
      (cons "recursive-fib-25" recursive-fib-25)
      (cons "odd-even-1000000" odd-even-1000000)
      (cons "factorial-100000" factorial-100000)))

  (define (benchmark evaluator-name)
    (define (loop experiments)
      (if (null? experiments)
          'done
          (let ((experiment-name (caar experiments))
                (code (cdar experiments)))
            (printf "~a ~a: " evaluator-name experiment-name)
            (time (evaluate code (setup-environment)))
            (loop (cdr experiments)))))
    (loop experiments))

  (load-relative "showcase/evaluator/evaluator.scm")
  (benchmark "evaluator")

  (load-relative "./showcase/analyzing/evaluator.scm")
  (benchmark "analyzing"))

(require 'benchmark)
