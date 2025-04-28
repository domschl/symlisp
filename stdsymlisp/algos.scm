(define (factorial n) (if (<= n 1) 1 (* n (factorial (- n 1)))))
(define (fibonacci n)
  (if (<= n 1)
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))
;(define (gcd a b)
;  (if (= b 0)
;      a
;      (gcd b (mod a b))))
