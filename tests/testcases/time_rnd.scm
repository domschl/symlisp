(define-test "current-time" (assert-true (< 0 (current-time))))
(define-test "gettimeofday-s-usec" (assert-true (pair? (gettimeofday))))
(define-test "prime-numbers-bench" (assert-true (pair? (time (next-prime 100)))))
(define-test "random-integer" (assert-true (integer? (random-integer 100))))
