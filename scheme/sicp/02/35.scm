; SICP exercise 2.35
;
; Redefine count-leaves from section 2.2.2 as an accumulation:
;
; (define (count-leaves t)
;   (accumulate <??> <??> (map <??> <??>)))

(define (count-leaves tree)
  (accumulate +
              0
              (map (lambda (x) (if (pair? x) (count-leaves x) 1))
                   tree)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
