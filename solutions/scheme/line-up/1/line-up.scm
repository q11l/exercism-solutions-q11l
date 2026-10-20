(import (rnrs))

(define sufix10 (lambda (number)
  (let ([last (modulo number 10)])
  (cond
    [(= 1 last) "st"]
    [(= 2 last) "nd"]
    [(= 3 last) "rd"]
    [else "th"]))))

(define sufix (lambda (number)
    (cond 
      [(= 11 number) "th"]
      [(= 0  number) "th"]
      [(= 12 number) "th"]
      [(= 13 number) "th"]
      [(= 112 number) "th"]
      [else (sufix10 number)])))

(define as-ordinal (lambda (number)
  (let ([suf (sufix number)]
        [to-str (number->string number)])
  (string-append to-str suf))))

(define format-message (lambda (name number)
  (let* ([text1 (string-append name ", you are the ")]
        [ord (as-ordinal number)]
        [text2 (string-append ord " customer we serve today. Thank you!")])
    (string-append text1 text2))))