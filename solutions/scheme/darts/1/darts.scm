(import (rnrs))

(define distance (lambda (x y) 
  (let* ([x2  (expt x 2)]
         [y2  (expt y 2)]
         [min (+  x2 y2)]
         [absolute (abs min)])
    (sqrt absolute))))

(define score (lambda (x y)
  (let ([d (distance x y)]) 
    (cond
      [(< 10  d)  0]
      [(< 5   d)  1]
      [(< 1   d)  5]
      [(<= 0  d) 10]))))