(import (rnrs))
 
(define leap-year? (lambda (year)
   (let ([four         (= (modulo year 4  ) 0)]
         [hundred      (= (modulo year 100) 0)]
         [four-hundred (= (modulo year 400) 0)])
   (and four (or (not hundred) four-hundred)))))