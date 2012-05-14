; SICP exercise 2.70
;
; The following eight-symbol alphabet with associated relative frequencies was
; designed to efficiently encode the lyrics of 1950s rock songs. (Note that
; "symbols" of an "alphabet" need not be individual letters.)
;
;   A    2   NA  16
;   BOOM 1   SHA  3
;   GET  2   YIP  9
;   JOB  2   WAH  1
;
; Use generate-huffman-tree (exercise 2.69) to generate a corresponding
; Huffman-tree, and use encode (exercise 2.68) to encode the following
; message:
;
;   Get a job
;   Sha na na na na na na na na
;   Get a job
;   Sha na na na na na na na na
;   Wah yip yip yip yip yip yip yip yip yip
;   Sha boom
;
; How many bits are required for the encoding? What is the smallest number of
; bits that would be needed to encode this song if we used a fix-length code
; for the eight symbol alphabet?

; The encoded message has 84 bits. If we used a fixed-length code, each symbol
; would require at least 3 bits (because there are 8 symbols). The message has
; 36 symbols, which makes the smallest number of bits that would be needed to
; encode the message equal to 108.
;
; You can run this file to verify.

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



(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cadr pair))
                    (make-leaf-set (cdr pairs))))))



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



(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (if (null? (cdr leaf-set))
      (car leaf-set)
      (let ((first (car leaf-set))
            (second (cadr leaf-set))
            (rest (cddr leaf-set)))
        (successive-merge (adjoin-set (make-code-tree first second)
                                      rest)))))


(define tree
  (generate-huffman-tree '((a 2)
                           (na 16)
                           (boom 1)
                           (sha 3)
                           (get 2)
                           (yip 9)
                           (job 2)
                           (wah 1))))

(define message
  '(get a job
    sha na na na na na na na na
    get a job
    sha na na na na na na na na
    wah yip yip yip yip yip yip yip yip yip
    sha boom))

(define encoded-message (encode message tree))

(printf "The encoded message is ~a\n" encoded-message)
(printf "It has ~a bits\n" (length encoded-message))
(printf "The message has ~a symbols\n" (length message))
(printf "This means, that a fixed-length code would take ~a bits\n" (* (length message) 3))
