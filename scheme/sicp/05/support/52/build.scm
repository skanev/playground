; The necessary code to compile the compiled interpreter and run the tests
; withn it

(require racket/runtime-path)

(define-runtime-path base-path ".")

(define (relative-path path)
  (find-relative-path
    (current-directory)
    (simplify-path (build-path base-path path))))

(define source-path (relative-path "runtime.c"))
(define tests-path (relative-path "tests.scm"))
(define compiled-code-path (relative-path "../bin/compiled_interpreter"))
(define target-path (relative-path "../bin/52"))

(define compile-call
  (format "cc --std=c99 ~a -o ~a"
          (path->string source-path)
          (path->string target-path)))

(define run-tests-call
  (format "~a ~a"
          (path->string target-path)
          (path->string tests-path)))

(define (write-interpreter c-code)
  (with-output-to-file compiled-code-path
     (lambda () (display c-code))
     #:mode 'text
     #:exists 'truncate))

(define (compile-runtime)
  (let ((exit-code (system/exit-code compile-call)))
    (when (not (zero? exit-code))
          (error "Failed to compile the runtime ;("))))

(define (run-interpreter-tests)
  (system run-tests-call))

(define (interpreter-test-results)
  (with-output-to-string run-interpreter-tests))
