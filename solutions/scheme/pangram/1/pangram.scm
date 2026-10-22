(import (rnrs))

(define build-alphabet (lambda (i)
  (let ([c (integer->char i)])
    (cons c
      (if (equal? c #\z) '()
        (build-alphabet (+ i 1)))))))
  
(define loop-alphabet (lambda (phrase alphabet)
  (if (null? alphabet) #true
      (if (null? phrase) #false
          (let* ([p  (car phrase)]
                 [ps (cdr phrase)]
                 [r  (remq p alphabet)])
            (loop-alphabet ps r))))))
  
(define pangram? (lambda (phrase)
  (let* ([lower       (string-downcase phrase)]
         [alphabet    (build-alphabet (char->integer #\a))]
         [phrase-list (string->list lower)])
    (loop-alphabet phrase-list alphabet))))