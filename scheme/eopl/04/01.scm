; EOPL exercise 4.01

; What would have happened had the program been instead
;
;   let g = proc (dummy)
;             let counter = newref (0)
;             in begin
;                  setref(counter, -(deref(counter), -1));
;                  deref(counter)
;                end
;   in let a = (g 11)
;      in let b = (g 11)
;         in -(a, b)

; Each invocation of g will create a new location in memory, initialize it to
; 0, increment it and return it. The result of the whole program will be 0.
