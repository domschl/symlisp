(define-test "current-time" (lambda () (assert-true (< 0 (current-time)))))
(define-test "gettimeofday-s-usec" (lambda () (assert-true (pair? (gettimeofday)))))
(define-test "prime-numbers-bench" (lambda () (assert-true (pair? (time (next-prime 100))))))
(define-test "random-integer" (lambda () (assert-true (integer? (random-integer 100)))))
