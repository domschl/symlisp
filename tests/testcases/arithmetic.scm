;;; Tests for Arithmetic Operations

;; --- + ---

(define-test "+-simple" (lambda () (assert-equal (+ 1 2) 3)))
(define-test "+-multi" (lambda () (assert-equal (+ 1 2 3 4 5) 15)))
(define-test "+-zero-args" (lambda () (assert-equal (+) 0)))
(define-test "+-one-arg" (lambda () (assert-equal (+ 10) 10)))
(define-test "+-negatives" (lambda () (assert-equal (+ -1 -2 -3) -6)))
(define-test "+-mixed-sign" (lambda () (assert-equal (+ 10 -5 3 -2) 6)))
(define-test "+-fractions" (lambda () (assert-equal (+ 1/2 1/4) 3/4)))
(define-test "+-fraction-integer" (lambda () (assert-equal (+ 1/2 1) 3/2)))
(define-test "+-result-simplify" (lambda () (assert-equal (+ 1/2 1/2) 1)))

;; --- - ---

(define-test "--simple" (lambda () (assert-equal (- 10 4) 6)))
(define-test "--unary-negation" (lambda () (assert-equal (- 10) -10)))
(define-test "--unary-negation-neg" (lambda () (assert-equal (- -5) 5)))
(define-test "--multi" (lambda () (assert-equal (- 20 5 3 2) 10)))
(define-test "--negatives" (lambda () (assert-equal (- -5 -3) -2)))
(define-test "--mixed-sign" (lambda () (assert-equal (- 10 -5) 15)))
(define-test "--fractions" (lambda () (assert-equal (- 3/4 1/4) 1/2)))
(define-test "--fraction-integer" (lambda () (assert-equal (- 2 1/2) 3/2)))
(define-test "--result-simplify" (lambda () (assert-equal (- 5/2 1/2) 2)))
(define-test "--unary-fraction" (lambda () (assert-equal (- 1/3) -1/3)))

;; --- * ---

(define-test "*-simple" (lambda () (assert-equal (* 3 4) 12)))
(define-test "*-multi" (lambda () (assert-equal (* 1 2 3 4 5) 120)))
(define-test "*-zero-args" (lambda () (assert-equal (*) 1)))
(define-test "*-one-arg" (lambda () (assert-equal (* 10) 10)))
(define-test "*-with-zero" (lambda () (assert-equal (* 10 5 0 3) 0)))
(define-test "*-negatives" (lambda () (assert-equal (* -2 -3 4) 24)))
(define-test "*-negatives-odd" (lambda () (assert-equal (* -2 -3 -4) -24)))
(define-test "*-fractions" (lambda () (assert-equal (* 1/2 2/3) 1/3)))
(define-test "*-fraction-integer" (lambda () (assert-equal (* 3/4 2) 3/2)))
(define-test "*-result-simplify" (lambda () (assert-equal (* 2/3 3/2) 1)))

;; --- / ---

(define-test "/-simple" (lambda () (assert-equal (/ 10 2) 5)))
(define-test "/-unary-reciprocal" (lambda () (assert-equal (/ 4) 1/4)))
(define-test "/-unary-reciprocal-fraction" (lambda () (assert-equal (/ 2/3) 3/2)))
(define-test "/-multi" (lambda () (assert-equal (/ 120 2 3 4) 5))) ; 120 / 2 / 3 / 4
(define-test "/-negatives" (lambda () (assert-equal (/ -10 2) -5)))
(define-test "/-negatives-both" (lambda () (assert-equal (/ -10 -2) 5)))
(define-test "/-fractions" (lambda () (assert-equal (/ 1/2 1/4) 2)))
(define-test "/-fraction-integer" (lambda () (assert-equal (/ 5 2) 5/2)))
(define-test "/-integer-fraction" (lambda () (assert-equal (/ 2 1/3) 6)))
(define-test "/-result-fraction" (lambda () (assert-equal (/ 3 2) 3/2)))

;; --- REMAINDER --- (Truncate division)

(define-test "remainder-positive" (lambda () (assert-equal (remainder 10 3) 1)))
(define-test "remainder-neg-dividend" (lambda () (assert-equal (remainder -10 3) -1))) ; Sign matches dividend
(define-test "remainder-neg-divisor" (lambda () (assert-equal (remainder 10 -3) 1))) ; Sign matches dividend
(define-test "remainder-neg-both" (lambda () (assert-equal (remainder -10 -3) -1))) ; Sign matches dividend
(define-test "remainder-exact" (lambda () (assert-equal (remainder 10 2) 0)))

;; --- MODULO --- (Floor division)

(define-test "modulo-positive" (lambda () (assert-equal (modulo 10 3) 1)))
(define-test "modulo-neg-dividend" (lambda () (assert-equal (modulo -10 3) 2))) ; Sign matches divisor (positive)
(define-test "modulo-neg-divisor" (lambda () (assert-equal (modulo 10 -3) -2))) ; Sign matches divisor (negative)
(define-test "modulo-neg-both" (lambda () (assert-equal (modulo -10 -3) -1))) ; Sign matches divisor (negative)
(define-test "modulo-exact" (lambda () (assert-equal (modulo 10 2) 0)))

;; --- Nested ---

(define-test "nested-arithmetic-1" (lambda () (assert-equal (+ (* 2 3) (- 10 5)) 11)))
(define-test "nested-arithmetic-2" (lambda () (assert-equal (/ (+ 1 2 3) (* 1 2)) 3)))
(define-test "nested-arithmetic-fractions" (lambda () (assert-equal (+ 1/2 (* 1/3 (- 5/2 1/2))) 7/6)))

;; --- DENOMINATOR ---

(define-test "denominator-integer" (lambda () (assert-equal (denominator 5) 1)))
(define-test "denominator-fraction" (lambda () (assert-equal (denominator 3/4) 4)))
(define-test "denominator-neg-fraction" (lambda () (assert-equal (denominator -2/3) 3))) ; Denominator is positive
(define-test "denominator-simplified" (lambda () (assert-equal (denominator 6/3) 1))) ; Simplifies to 2/1
(define-test "denominator-zero" (lambda () (assert-equal (denominator 0) 1)))

;; --- NUMERATOR ---

(define-test "numerator-integer" (lambda () (assert-equal (numerator 5) 5)))
(define-test "numerator-fraction" (lambda () (assert-equal (numerator 3/4) 3)))
(define-test "numerator-neg-fraction" (lambda () (assert-equal (numerator -2/3) -2))) ; Numerator holds sign
(define-test "numerator-simplified" (lambda () (assert-equal (numerator 6/3) 2))) ; Simplifies to 2/1
(define-test "numerator-zero" (lambda () (assert-equal (numerator 0) 0)))

;; --- QUOTIENT --- (Integer division, truncates towards zero)

(define-test "quotient-positive" (lambda () (assert-equal (quotient 10 3) 3)))
(define-test "quotient-neg-dividend" (lambda () (assert-equal (quotient -10 3) -3)))
(define-test "quotient-neg-divisor" (lambda () (assert-equal (quotient 10 -3) -3)))
(define-test "quotient-neg-both" (lambda () (assert-equal (quotient -10 -3) 3)))
(define-test "quotient-exact" (lambda () (assert-equal (quotient 12 4) 3)))
(define-test "quotient-zero-dividend" (lambda () (assert-equal (quotient 0 5) 0)))

;; --- GCD --- (Greatest Common Divisor)

(define-test "gcd-two-pos" (lambda () (assert-equal (gcd 48 18) 6)))
(define-test "gcd-multi-pos" (lambda () (assert-equal (gcd 48 18 30) 6)))
(define-test "gcd-primes" (lambda () (assert-equal (gcd 7 13) 1)))
(define-test "gcd-with-zero" (lambda () (assert-equal (gcd 12 0) 12)))
(define-test "gcd-with-zero-multi" (lambda () (assert-equal (gcd 12 0 8) 4)))
(define-test "gcd-negatives" (lambda () (assert-equal (gcd -48 18) 6)))
(define-test "gcd-negatives-both" (lambda () (assert-equal (gcd -48 -18) 6)))
(define-test "gcd-one-arg" (lambda () (assert-equal (gcd 15) 15)))
(define-test "gcd-one-arg-neg" (lambda () (assert-equal (gcd -15) 15))) ; Result is non-negative
(define-test "gcd-no-args" (lambda () (assert-equal (gcd) 0)))
(define-test "gcd-multiple" (lambda () (assert-equal (gcd 12 24 6) 6)))

;; --- LCM --- (Least Common Multiple)

(define-test "lcm-two-pos" (lambda () (assert-equal (lcm 12 18) 36)))
(define-test "lcm-multi-pos" (lambda () (assert-equal (lcm 2 3 4) 12)))
(define-test "lcm-primes" (lambda () (assert-equal (lcm 7 13) 91)))
(define-test "lcm-with-zero" (lambda () (assert-equal (lcm 12 0) 0)))
(define-test "lcm-with-zero-multi" (lambda () (assert-equal (lcm 12 0 8) 0)))
(define-test "lcm-negatives" (lambda () (assert-equal (lcm -12 18) 36)))
(define-test "lcm-negatives-both" (lambda () (assert-equal (lcm -12 -18) 36)))
(define-test "lcm-one-arg" (lambda () (assert-equal (lcm 15) 15)))
(define-test "lcm-one-arg-neg" (lambda () (assert-equal (lcm -15) 15))) ; Result is non-negative
(define-test "lcm-no-args" (lambda () (assert-equal (lcm) 1)))
(define-test "lcm-multiple" (lambda () (assert-equal (lcm 12 24 6) 24)))

;; --- ABS ---

(define-test "abs-positive-int" (lambda () (assert-equal (abs 5) 5)))
(define-test "abs-negative-int" (lambda () (assert-equal (abs -10) 10)))
(define-test "abs-zero" (lambda () (assert-equal (abs 0) 0)))
(define-test "abs-positive-fraction" (lambda () (assert-equal (abs 3/4) 3/4)))
(define-test "abs-negative-fraction" (lambda () (assert-equal (abs -2/3) 2/3)))
(define-test "abs-simplified-fraction" (lambda () (assert-equal (abs -6/3) 2))) ; Simplifies to -2, then abs is 2

;; --- MAX ---

(define-test "max-two-pos" (lambda () (assert-equal (max 10 5) 10)))
(define-test "max-multi-pos" (lambda () (assert-equal (max 1 5 3 8 2) 8)))
(define-test "max-negatives" (lambda () (assert-equal (max -10 -5 -20) -5)))
(define-test "max-mixed-sign" (lambda () (assert-equal (max -10 5 -20 0) 5)))
(define-test "max-one-arg" (lambda () (assert-equal (max 42) 42)))
(define-test "max-fractions" (lambda () (assert-equal (max 1/2 3/4 1/4) 3/4)))
(define-test "max-mixed-types" (lambda () (assert-equal (max 1 3/2 0 -1) 3/2)))
(define-test "max-equal-values" (lambda () (assert-equal (max 5 5 5) 5)))

;; --- MIN ---

(define-test "min-two-pos" (lambda () (assert-equal (min 10 5) 5)))
(define-test "min-multi-pos" (lambda () (assert-equal (min 1 5 3 8 2) 1)))
(define-test "min-negatives" (lambda () (assert-equal (min -10 -5 -20) -20)))
(define-test "min-mixed-sign" (lambda () (assert-equal (min -10 5 -20 0) -20)))
(define-test "min-one-arg" (lambda () (assert-equal (min 42) 42)))
(define-test "min-fractions" (lambda () (assert-equal (min 1/2 3/4 1/4) 1/4)))
(define-test "min-mixed-types" (lambda () (assert-equal (min 1 3/2 0 -1) -1)))
(define-test "min-equal-values" (lambda () (assert-equal (min 5 5 5) 5)))

;; --- EXPT ---

(define-test "expt-pos-int" (lambda () (assert-equal (expt 2 10) 1024)))
(define-test "expt-base-1" (lambda () (assert-equal (expt 1 100) 1)))
(define-test "expt-exp-1" (lambda () (assert-equal (expt 123 1) 123)))
(define-test "expt-exp-0" (lambda () (assert-equal (expt 123 0) 1)))
(define-test "expt-base-0-pos-exp" (lambda () (assert-equal (expt 0 5) 0)))
(define-test "expt-base-0-exp-0" (lambda () (assert-equal (expt 0 0) 1))) ; Scheme standard
(define-test "expt-neg-base-even-exp" (lambda () (assert-equal (expt -2 4) 16)))
(define-test "expt-neg-base-odd-exp" (lambda () (assert-equal (expt -2 3) -8)))
(define-test "expt-fraction-base" (lambda () (assert-equal (expt 1/2 3) 1/8)))
(define-test "expt-neg-exp" (lambda () (assert-equal (expt 2 -3) 1/8)))
(define-test "expt-fraction-neg-exp" (lambda () (assert-equal (expt 2/3 -2) 9/4)))
(define-test "expt-neg-base-neg-exp" (lambda () (assert-equal (expt -3 -3) -1/27)))
; Error cases (optional, depending on how you want errors handled/tested)
; (define-test "expt-error-0-neg-exp" (lambda () (assert-error? (expt 0 -2))))
; (define-test "expt-error-non-integer-exp" (lambda () (assert-error? (expt 2 1/2))))

;; --- SQUARE ---

(define-test "square-pos-int" (lambda () (assert-equal (square 5) 25)))
(define-test "square-neg-int" (lambda () (assert-equal (square -4) 16)))
(define-test "square-zero" (lambda () (assert-equal (square 0) 0)))
(define-test "square-pos-fraction" (lambda () (assert-equal (square 2/3) 4/9)))
(define-test "square-neg-fraction" (lambda () (assert-equal (square -3/2) 9/4)))
(define-test "square-simplify" (lambda () (assert-equal (square 6/3) 4))) ; 6/3 simplifies to 2

;; --- EXACT-INTEGER-SQRT ---

(define-test "eisqrt-perfect-square" (lambda () (assert-equal (exact-integer-sqrt 25) '(5 0))))
(define-test "eisqrt-non-perfect" (lambda () (assert-equal (exact-integer-sqrt 30) '(5 5)))) ; 5*5 + 5 = 30
(define-test "eisqrt-zero" (lambda () (assert-equal (exact-integer-sqrt 0) '(0 0))))
(define-test "eisqrt-one" (lambda () (assert-equal (exact-integer-sqrt 1) '(1 0))))
(define-test "eisqrt-large" (lambda () (assert-equal (exact-integer-sqrt 123456789) '(11111 2468))))
; Error case (optional)
; (define-test "eisqrt-error-negative" (lambda () (assert-error? (exact-integer-sqrt -10))))

;; --- FLOAT ---

(define-test "float-int-default" (lambda () (assert-equal (float 123) "123.0000000000"))) ; Default 10 places
(define-test "float-int-prec-3" (lambda () (assert-equal (float 123 3) "123.000")))
(define-test "float-int-prec-0" (lambda () (assert-equal (float 123 0) "123")))
(define-test "float-neg-int-default" (lambda () (assert-equal (float -45) "-45.0000000000")))
(define-test "float-neg-int-prec-2" (lambda () (assert-equal (float -45 2) "-45.00")))
(define-test "float-zero-default" (lambda () (assert-equal (float 0) "0.0000000000")))
(define-test "float-zero-prec-0" (lambda () (assert-equal (float 0 0) "0")))

(define-test "float-fraction-1/2-default" (lambda () (assert-equal (float 1/2) "0.5000000000")))
(define-test "float-fraction-1/2-prec-3" (lambda () (assert-equal (float 1/2 3) "0.500")))
(define-test "float-fraction-1/3-default" (lambda () (assert-equal (float 1/3) "0.3333333333"))) ; Rounded
(define-test "float-fraction-1/3-prec-5" (lambda () (assert-equal (float 1/3 5) "0.33333")))
(define-test "float-fraction-2/3-default" (lambda () (assert-equal (float 2/3) "0.6666666667"))) ; Rounded up
(define-test "float-fraction-2/3-prec-4" (lambda () (assert-equal (float 2/3 4) "0.6667")))
(define-test "float-fraction-neg-1/4-default" (lambda () (assert-equal (float -1/4) "-0.2500000000")))
(define-test "float-fraction-neg-1/4-prec-1" (lambda () (assert-equal (float -1/4 1) "-0.3"))) ; Rounded
(define-test "float-fraction-neg-1/4-prec-2" (lambda () (assert-equal (float -1/4 2) "-0.25")))
(define-test "float-fraction-small-prec-5" (lambda () (assert-equal (float 1/8 5) "0.12500")))
(define-test "float-fraction-very-small-prec-5" (lambda () (assert-equal (float 1/1000 5) "0.00100")))
(define-test "float-fraction-mixed-prec-3" (lambda () (assert-equal (float 123/8 3) "15.375")))
(define-test "float-fraction-mixed-round-prec-2" (lambda () (assert-equal (float 123/8 2) "15.38"))) ; 15.375 rounds up

;;; --- END OF ARITHMETIC TESTS ---