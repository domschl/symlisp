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

;; --- DENOMINATOR ---
(define-test "denominator-integer" (assert-equal (denominator 5) 1))
(define-test "denominator-fraction" (assert-equal (denominator 3/4) 4))
(define-test "denominator-neg-fraction" (assert-equal (denominator -2/3) 3)) ; Denominator is positive
(define-test "denominator-simplified" (assert-equal (denominator 6/3) 1)) ; Simplifies to 2/1
(define-test "denominator-zero" (assert-equal (denominator 0) 1))

;; --- NUMERATOR ---
(define-test "numerator-integer" (assert-equal (numerator 5) 5))
(define-test "numerator-fraction" (assert-equal (numerator 3/4) 3))
(define-test "numerator-neg-fraction" (assert-equal (numerator -2/3) -2)) ; Numerator holds sign
(define-test "numerator-simplified" (assert-equal (numerator 6/3) 2)) ; Simplifies to 2/1
(define-test "numerator-zero" (assert-equal (numerator 0) 0))

;; --- QUOTIENT --- (Integer division, truncates towards zero)
(define-test "quotient-positive" (assert-equal (quotient 10 3) 3))
(define-test "quotient-neg-dividend" (assert-equal (quotient -10 3) -3))
(define-test "quotient-neg-divisor" (assert-equal (quotient 10 -3) -3))
(define-test "quotient-neg-both" (assert-equal (quotient -10 -3) 3))
(define-test "quotient-exact" (assert-equal (quotient 12 4) 3))
(define-test "quotient-zero-dividend" (assert-equal (quotient 0 5) 0))

;; --- GCD --- (Greatest Common Divisor)
(define-test "gcd-two-pos" (assert-equal (gcd 48 18) 6))
(define-test "gcd-multi-pos" (assert-equal (gcd 48 18 30) 6))
(define-test "gcd-primes" (assert-equal (gcd 7 13) 1))
(define-test "gcd-with-zero" (assert-equal (gcd 12 0) 12))
(define-test "gcd-with-zero-multi" (assert-equal (gcd 12 0 8) 4))
(define-test "gcd-negatives" (assert-equal (gcd -48 18) 6))
(define-test "gcd-negatives-both" (assert-equal (gcd -48 -18) 6))
(define-test "gcd-one-arg" (assert-equal (gcd 15) 15))
(define-test "gcd-one-arg-neg" (assert-equal (gcd -15) 15)) ; Result is non-negative
(define-test "gcd-no-args" (assert-equal (gcd) 0))
(define-test "gcd-multiple" (assert-equal (gcd 12 24 6) 6))

;; --- LCM --- (Least Common Multiple)
(define-test "lcm-two-pos" (assert-equal (lcm 12 18) 36))
(define-test "lcm-multi-pos" (assert-equal (lcm 2 3 4) 12))
(define-test "lcm-primes" (assert-equal (lcm 7 13) 91))
(define-test "lcm-with-zero" (assert-equal (lcm 12 0) 0))
(define-test "lcm-with-zero-multi" (assert-equal (lcm 12 0 8) 0))
(define-test "lcm-negatives" (assert-equal (lcm -12 18) 36))
(define-test "lcm-negatives-both" (assert-equal (lcm -12 -18) 36))
(define-test "lcm-one-arg" (assert-equal (lcm 15) 15))
(define-test "lcm-one-arg-neg" (assert-equal (lcm -15) 15)) ; Result is non-negative
(define-test "lcm-no-args" (assert-equal (lcm) 1))
(define-test "lcm-multiple" (assert-equal (lcm 12 24 6) 24))

;; --- ABS ---
(define-test "abs-positive-int" (assert-equal (abs 5) 5))
(define-test "abs-negative-int" (assert-equal (abs -10) 10))
(define-test "abs-zero" (assert-equal (abs 0) 0))
(define-test "abs-positive-fraction" (assert-equal (abs 3/4) 3/4))
(define-test "abs-negative-fraction" (assert-equal (abs -2/3) 2/3))
(define-test "abs-simplified-fraction" (assert-equal (abs -6/3) 2)) ; Simplifies to -2, then abs is 2

;; --- MAX ---
(define-test "max-two-pos" (assert-equal (max 10 5) 10))
(define-test "max-multi-pos" (assert-equal (max 1 5 3 8 2) 8))
(define-test "max-negatives" (assert-equal (max -10 -5 -20) -5))
(define-test "max-mixed-sign" (assert-equal (max -10 5 -20 0) 5))
(define-test "max-one-arg" (assert-equal (max 42) 42))
(define-test "max-fractions" (assert-equal (max 1/2 3/4 1/4) 3/4))
(define-test "max-mixed-types" (assert-equal (max 1 3/2 0 -1) 3/2))
(define-test "max-equal-values" (assert-equal (max 5 5 5) 5))

;; --- MIN ---
(define-test "min-two-pos" (assert-equal (min 10 5) 5))
(define-test "min-multi-pos" (assert-equal (min 1 5 3 8 2) 1))
(define-test "min-negatives" (assert-equal (min -10 -5 -20) -20))
(define-test "min-mixed-sign" (assert-equal (min -10 5 -20 0) -20))
(define-test "min-one-arg" (assert-equal (min 42) 42))
(define-test "min-fractions" (assert-equal (min 1/2 3/4 1/4) 1/4))
(define-test "min-mixed-types" (assert-equal (min 1 3/2 0 -1) -1))
(define-test "min-equal-values" (assert-equal (min 5 5 5) 5))

;; --- EXPT ---
(define-test "expt-pos-int" (assert-equal (expt 2 10) 1024))
(define-test "expt-base-1" (assert-equal (expt 1 100) 1))
(define-test "expt-exp-1" (assert-equal (expt 123 1) 123))
(define-test "expt-exp-0" (assert-equal (expt 123 0) 1))
(define-test "expt-base-0-pos-exp" (assert-equal (expt 0 5) 0))
(define-test "expt-base-0-exp-0" (assert-equal (expt 0 0) 1)) ; Scheme standard
(define-test "expt-neg-base-even-exp" (assert-equal (expt -2 4) 16))
(define-test "expt-neg-base-odd-exp" (assert-equal (expt -2 3) -8))
(define-test "expt-fraction-base" (assert-equal (expt 1/2 3) 1/8))
(define-test "expt-neg-exp" (assert-equal (expt 2 -3) 1/8))
(define-test "expt-fraction-neg-exp" (assert-equal (expt 2/3 -2) 9/4))
(define-test "expt-neg-base-neg-exp" (assert-equal (expt -3 -3) -1/27))
; Error cases (optional, depending on how you want errors handled/tested)
; (define-test "expt-error-0-neg-exp" (assert-error? (expt 0 -2)))
; (define-test "expt-error-non-integer-exp" (assert-error? (expt 2 1/2)))

;; --- SQUARE ---
(define-test "square-pos-int" (assert-equal (square 5) 25))
(define-test "square-neg-int" (assert-equal (square -4) 16))
(define-test "square-zero" (assert-equal (square 0) 0))
(define-test "square-pos-fraction" (assert-equal (square 2/3) 4/9))
(define-test "square-neg-fraction" (assert-equal (square -3/2) 9/4))
(define-test "square-simplify" (assert-equal (square 6/3) 4)) ; 6/3 simplifies to 2

;; --- EXACT-INTEGER-SQRT ---
(define-test "eisqrt-perfect-square" (assert-equal (exact-integer-sqrt 25) '(5 0)))
(define-test "eisqrt-non-perfect" (assert-equal (exact-integer-sqrt 30) '(5 5))) ; 5*5 + 5 = 30
(define-test "eisqrt-zero" (assert-equal (exact-integer-sqrt 0) '(0 0)))
(define-test "eisqrt-one" (assert-equal (exact-integer-sqrt 1) '(1 0)))
(define-test "eisqrt-large" (assert-equal (exact-integer-sqrt 123456789) '(11111 2468)))
; Error case (optional)
; (define-test "eisqrt-error-negative" (assert-error? (exact-integer-sqrt -10)))

;; --- FLOAT ---
(define-test "float-int-default" (assert-equal (float 123) "123.0000000000")) ; Default 10 places
(define-test "float-int-prec-3" (assert-equal (float 123 3) "123.000"))
(define-test "float-int-prec-0" (assert-equal (float 123 0) "123"))
(define-test "float-neg-int-default" (assert-equal (float -45) "-45.0000000000"))
(define-test "float-neg-int-prec-2" (assert-equal (float -45 2) "-45.00"))
(define-test "float-zero-default" (assert-equal (float 0) "0.0000000000"))
(define-test "float-zero-prec-0" (assert-equal (float 0 0) "0"))

(define-test "float-fraction-1/2-default" (assert-equal (float 1/2) "0.5000000000"))
(define-test "float-fraction-1/2-prec-3" (assert-equal (float 1/2 3) "0.500"))
(define-test "float-fraction-1/3-default" (assert-equal (float 1/3) "0.3333333333")) ; Rounded
(define-test "float-fraction-1/3-prec-5" (assert-equal (float 1/3 5) "0.33333"))
(define-test "float-fraction-2/3-default" (assert-equal (float 2/3) "0.6666666667")) ; Rounded up
(define-test "float-fraction-2/3-prec-4" (assert-equal (float 2/3 4) "0.6667"))
(define-test "float-fraction-neg-1/4-default" (assert-equal (float -1/4) "-0.2500000000"))
(define-test "float-fraction-neg-1/4-prec-1" (assert-equal (float -1/4 1) "-0.3")) ; Rounded
(define-test "float-fraction-neg-1/4-prec-2" (assert-equal (float -1/4 2) "-0.25"))
(define-test "float-fraction-small-prec-5" (assert-equal (float 1/8 5) "0.12500"))
(define-test "float-fraction-very-small-prec-5" (assert-equal (float 1/1000 5) "0.00100"))
(define-test "float-fraction-mixed-prec-3" (assert-equal (float 123/8 3) "15.375"))
(define-test "float-fraction-mixed-round-prec-2" (assert-equal (float 123/8 2) "15.38")) ; 15.375 rounds up

;;; --- END OF ARITHMETIC TESTS ---