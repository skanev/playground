; SICP exercise 4.41
;
; Write an ordinary Scheme program to solve the multiple dwelling puzzle.

; Wow, really? OK!

(define (multiple-dwellings)
  (define (solution? baker cooper fletcher miller smith)
    (and (not (= baker 5))
         (not (= cooper 1))
         (not (= fletcher 5))
         (not (= fletcher 1))
         (> miller cooper)
         (not (= (abs (- smith fletcher)) 1))
         (not (= (abs (- fletcher cooper)) 1))))
  (define (combine-lists a b)
    (if (null? a)
        '()
        (cons (list (car a) (car b))
              (combine-lists (cdr a) (cdr b)))))
  (map (lambda (floors) (combine-lists '(baker cooper fletcher miller smith) floors))
       (filter (lambda (floors) (apply solution? floors))
               (permutations 5))))

(define (permutations n)
  (define (insert-into n items)
    (if (null? items)
        (list (cons n '()))
        (cons (cons n items)
              (map (lambda (rest) (cons (car items) rest))
                   (insert-into n (cdr items))))))
  (if (= n 1)
      '((1))
      (flat-map (lambda (items) (insert-into n items))
                (permutations (- n 1)))))

(define (flat-map proc items)
  (if (null? items)
      '()
      (append (proc (car items))
              (flat-map proc (cdr items)))))
