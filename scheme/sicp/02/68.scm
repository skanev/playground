; SICP exercise 2.68
;
; The encode procedure takes as arguments a message and a tree and produces the
; list of bits that gives the encoded message.
;
; (define (encode message tree)
;   (if (null? message)
;       '()
;       (append (encode-symbol (car message) tree)
;               (encode (cdr message) tree))))
;
; encode-symbol is a procedure, which you must write, that returns the lists of
; bits that encodes a given symbol according to a given tree. You should design
; encode-symbol so that it signals an error if the symbol is not in the tree at
; all. The your procedure by encoding the result you obtained in exercise 2.67
; with the sample tree and seeing whether it is the same as the original sample
; message.

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x)
  (cadr x))

(define (weight-leaf x)
  (caddr x))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))



(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (let ((left (left-branch tree))
        (right (right-branch tree)))
    (cond ((leaf? tree) '())
          ((member symbol (symbols left)) (cons 0 (encode-symbol symbol left)))
          ((member symbol (symbols right)) (cons 1 (encode-symbol symbol right)))
          (else (error "bad symbol - ENCODE-SYMBOL" symbol)))))
