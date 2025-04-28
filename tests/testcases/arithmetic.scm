;;; Tests for Arithmetic Operations

;; --- + ---
(define-test "+-simple" (assert-equal (+ 1 2) 3))
(define-test "+-multi" (assert-equal (+ 1 2 3 4 5) 15))
(define-test "+-zero-args" (assert-equal (+) 0))
(define-test "+-one-arg" (assert-equal (+ 10) 10))
(define-test "+-negatives" (assert-equal (+ -1 -2 -3) -6))
(define-test "+-mixed-sign" (assert-equal (+ 10 -5 3 -2) 6))
(define-test "+-fractions" (assert-equal (+ 1/2 1/4) 3/4))
(define-test "+-fraction-integer" (assert-equal (+ 1/2 1) 3/2))
(define-test "+-result-simplify" (assert-equal (+ 1/2 1/2) 1))

;; --- - ---
(define-test "--simple" (assert-equal (- 10 4) 6))
(define-test "--unary-negation" (assert-equal (- 10) -10))
(define-test "--unary-negation-neg" (assert-equal (- -5) 5))
(define-test "--multi" (assert-equal (- 20 5 3 2) 10))
(define-test "--negatives" (assert-equal (- -5 -3) -2))
(define-test "--mixed-sign" (assert-equal (- 10 -5) 15))
(define-test "--fractions" (assert-equal (- 3/4 1/4) 1/2))
(define-test "--fraction-integer" (assert-equal (- 2 1/2) 3/2))
(define-test "--result-simplify" (assert-equal (- 5/2 1/2) 2))
(define-test "--unary-fraction" (assert-equal (- 1/3) -1/3))

;; --- * ---
(define-test "*-simple" (assert-equal (* 3 4) 12))
(define-test "*-multi" (assert-equal (* 1 2 3 4 5) 120))
(define-test "*-zero-args" (assert-equal (*) 1))
(define-test "*-one-arg" (assert-equal (* 10) 10))
(define-test "*-with-zero" (assert-equal (* 10 5 0 3) 0))
(define-test "*-negatives" (assert-equal (* -2 -3 4) 24))
(define-test "*-negatives-odd" (assert-equal (* -2 -3 -4) -24))
(define-test "*-fractions" (assert-equal (* 1/2 2/3) 1/3))
(define-test "*-fraction-integer" (assert-equal (* 3/4 2) 3/2))
(define-test "*-result-simplify" (assert-equal (* 2/3 3/2) 1))

;; --- / ---
(define-test "/-simple" (assert-equal (/ 10 2) 5))
(define-test "/-unary-reciprocal" (assert-equal (/ 4) 1/4))
(define-test "/-unary-reciprocal-fraction" (assert-equal (/ 2/3) 3/2))
(define-test "/-multi" (assert-equal (/ 120 2 3 4) 5)) ; 120 / 2 / 3 / 4
(define-test "/-negatives" (assert-equal (/ -10 2) -5))
(define-test "/-negatives-both" (assert-equal (/ -10 -2) 5))
(define-test "/-fractions" (assert-equal (/ 1/2 1/4) 2))
(define-test "/-fraction-integer" (assert-equal (/ 5 2) 5/2))
(define-test "/-integer-fraction" (assert-equal (/ 2 1/3) 6))
(define-test "/-result-fraction" (assert-equal (/ 3 2) 3/2))

;; --- REMAINDER --- (Truncate division)
(define-test "remainder-positive" (assert-equal (remainder 10 3) 1))
(define-test "remainder-neg-dividend" (assert-equal (remainder -10 3) -1)) ; Sign matches dividend
(define-test "remainder-neg-divisor" (assert-equal (remainder 10 -3) 1)) ; Sign matches dividend
(define-test "remainder-neg-both" (assert-equal (remainder -10 -3) -1)) ; Sign matches dividend
(define-test "remainder-exact" (assert-equal (remainder 10 2) 0))

;; --- MODULO --- (Floor division)
(define-test "modulo-positive" (assert-equal (modulo 10 3) 1))
(define-test "modulo-neg-dividend" (assert-equal (modulo -10 3) 2)) ; Sign matches divisor (positive)
(define-test "modulo-neg-divisor" (assert-equal (modulo 10 -3) -2)) ; Sign matches divisor (negative)
(define-test "modulo-neg-both" (assert-equal (modulo -10 -3) -1)) ; Sign matches divisor (negative)
(define-test "modulo-exact" (assert-equal (modulo 10 2) 0))

;; --- Nested ---
(define-test "nested-arithmetic-1" (assert-equal (+ (* 2 3) (- 10 5)) 11))
(define-test "nested-arithmetic-2" (assert-equal (/ (+ 1 2 3) (* 1 2)) 3))
(define-test "nested-arithmetic-fractions" (assert-equal (+ 1/2 (* 1/3 (- 5/2 1/2))) 7/6))
