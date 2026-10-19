(import (rnrs))

(define egg-count (lambda (number)
  (if (= 0 number) 0
    (+ (modulo number 2)
      (egg-count (quotient number 2))))))