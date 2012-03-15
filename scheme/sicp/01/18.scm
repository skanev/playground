; SICP exercise 1.18
;
; Using the results of exercises 1.16 and 1.17, devise a procedure that
; generates an iterative process for multiplying two integers in terms of
; adding, doubling and halving and uses a logarithmic number of steps.

; Say we want to multiply a by b. We are going to do this iteratively and our
; invariant quantitiy will be ab + c, with c initially being 0. We iteratively
; apply the following transformations:
;
; ab + c = { 2a(b/2) + c       if b is even
;          { a(b-1) + a + c    if b is odd
;
; We conclude the result is c when b is zero.

(define (double a)
  (+ a a))

(define (halve a)
  (/ a 2))

(define (** a b)
  (define (iter a b c)
    (cond ((= b 0) c)
          ((even? b) (iter (double a) (halve b) c))
          (else (iter a (- b 1) (+ a c)))))

  (iter a b 0))
