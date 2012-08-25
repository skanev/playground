; SICP exercise 2.81
;
; Louis Reasoner has noticed that apply-generic may try to coerce arguments to
; each other's types even if they already have the same type. Therefore, he
; reasons, we need to put procedures in the coercion table to coerce arguments
; of each type to their own type. For exapmle, in addition to the
; scheme-number->complex coercion shown above, he would do:
;
;   (define (scheme-number->scheme-number n) n)
;   (define (complex->complex z) z)
;   (put-coercion 'scheme-number 'scheme-number
;                 scheme-number->scheme-number)
;   (put-coercion 'complex 'complex complex->complex)
;
; a. With Louis' coercion procedures installed, what happens if apply-generic
; is called with two arguments of type scheme-number or two arguments of type
; complex for an operation that is not found in the table for those types? For
; example, assume that we've defined a generic exponentiation operation:
;
;   (define (exp x y) (apply-generic 'exp x y))
;
; and have put a procedure for exponentiation in the Scheme number package but
; not in any other package:
;
;   ;; following added to Scheme-number package
;   (put 'exp '(scheme-number scheme-number)
;        (lambda (x y) (tag (expt x y)))) ; using primitive expt
;
; What happens if we call exp with two complex numbers as arguments?
;
; b. Is Louis correct that something had to be done about coercion with
; arguments of the same type, or does apply-generic work correctly as is?
;
; c. Modify apply-generic so that it doesn't try coercion if the two arguments
; have the same type.

; a. Well, naturally, when exp is called with two complex numbers, it would
; not be found in the table. Since a coercion procedure is found, though, it
; would recursively call itself with the same two complex numbers, leading to
; an infinite recursion. Given the tail position of the recursive call, the
; program would just loop infinitely.
;
; b. He is right, although he does not provide a good reason for being right.
; Trying to coerce numbers to the same type does not hurt. It slows the
; computation down, but in no way makes it incorrect. The real problem is that
; installing those coercion procedures causes operations to end up in an
; infinite loop. What we can do is avoid coercing numbers that are the same
; type. That way installing those procedures (which are logically right) would
; not cause the program to get stuck.
;
; c. I will not provide a test for this, just the code. I hope I am forgiven,
; since otherwise I would need to drag in a lot of code in order to test it:

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (not (eq? type1 type2))
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2
                              (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                              (apply-generic op a1 (t2->t1 a2)))
                            (else
                              (error "No method for these types" (list op type-tags)))))
                    (error "No method for these types" (list op type-tags))))
              (error "No method for these types" (list op type-tags)))))))
