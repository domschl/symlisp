(define-test-thunked "current-time" (lambda () (assert-true (< 0 (current-time)))))
(define-test-thunked "gettimeofday-s-usec" (lambda () (assert-true (pair? (gettimeofday)))))
(define-test-thunked "prime-numbers-bench" (lambda () (assert-true (pair? (time (next-prime 100))))))
(define-test-thunked "random-integer" (lambda () (assert-true (integer? (random-integer 100)))))
