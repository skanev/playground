; SICP exercise 5.51
;
; Develop a rudimentary implementation of Scheme in C (or some other low-level
; language of your choice) by translating the explicit-control evaluator of
; section 5.4 into C. In order to run this code, you will need to also provide
; appropriate storage-allocation routines and other run-time support.

; That was a journey. It took me three days to implement this fully. More
; commentary can be found in 05/support/51/evaluator.c, which contains the
; actual C code to do it.
;
; This file provides with some necessary code in order to compile and run the
; interpreter. Note that it depends on cc --std=c99.

(require racket/runtime-path)

(define-runtime-path base-path ".")

(define (relative-path path)
  (find-relative-path
    (current-directory)
    (simplify-path (build-path base-path path))))

(define source-path (relative-path "support/51/evaluator.c"))
(define tests-path (relative-path "support/51/tests.scm"))
(define target-path (relative-path "support/bin/51"))

(define compile-call
  (format "cc --std=c99 ~a -o ~a"
          (path->string source-path)
          (path->string target-path)))

(define run-tests-call
  (format "~a ~a"
          (path->string target-path)
          (path->string tests-path)))

(define (compile-interpreter)
  (let ((exit-code (system/exit-code compile-call)))
    (when (not (zero? exit-code))
          (error "Failed to compile the interpreter ;("))))

(define (run-interpreter-tests)
  (system run-tests-call))

(define (interpreter-test-results)
  (with-output-to-string run-interpreter-tests))
