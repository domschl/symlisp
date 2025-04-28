(define-test "simple-addition" (assert-equal (+ 1 2) 3))

(define-test "multi-add" (assert-equal (+ 1 2 3 4 5) 15))

(define-test "subtraction" (assert-equal (- 10 4) 6))

(define-test "multiplication" (assert-equal (* 3 4) 12))

(define-test "division" (assert-equal (/ 10 2) 5))

(define-test "nested-arithmetic" (assert-equal (+ (* 2 3) (- 10 5)) 11))

