(define (factorial n) (if (<= n 1) 1 (* n (factorial (- n 1)))))

(define (fibonacci n)
  (if (<= n 1)
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(define (sqrt n) (car (exact-integer-sqrt n)))

;; Get n-th prime number:
(define (is-prime? n)
  (if (< n 2)
      #f
      (let loop ((i 2))
        (if (> i (sqrt n))
            #t
            (if (= (modulo n i) 0)
                #f
                (loop (+ i 1)))))))

(define (nth-prime-helper n count current)
  (if (= count n)
      (- current 1)
      (if (is-prime? current)
          (nth-prime-helper n (+ count 1) (+ current 1))
          (nth-prime-helper n count (+ current 1)))))
(define (nth-prime n)
  (if (<= n 0)
      (display "n must be a positive integer")
      (nth-prime-helper n 1 3)))
