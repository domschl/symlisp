;;;-----------------------------------------------------------------------------
;;; Number Theory Functions
;;;-----------------------------------------------------------------------------

;;; prime-factors Tests
(define-test "prime-factors-0" (assert-equal '() (prime-factors 0)))
(define-test "prime-factors-1" (assert-equal '() (prime-factors 1)))
(define-test "prime-factors-neg-1" (assert-equal '() (prime-factors -1)))
(define-test "prime-factors-2" (assert-equal '(2) (prime-factors 2)))
(define-test "prime-factors-3" (assert-equal '(3) (prime-factors 3)))
(define-test "prime-factors-4" (assert-equal '(2 2) (prime-factors 4)))
(define-test "prime-factors-6" (assert-equal '(2 3) (prime-factors 6)))
(define-test "prime-factors-12" (assert-equal '(2 2 3) (prime-factors 12)))
(define-test "prime-factors-17" (assert-equal '(17) (prime-factors 17)))
(define-test "prime-factors-100" (assert-equal '(2 2 5 5) (prime-factors 100)))
(define-test "prime-factors-99" (assert-equal '(3 3 11) (prime-factors 99)))
(define-test "prime-factors-large-prime" (assert-equal '(7919) (prime-factors 7919)))
(define-test "prime-factors-large-composite" (assert-equal '(89 89) (prime-factors 7921))) ; 89*89
(define-test "prime-factors-mersenne-31" (assert-equal '(2147483647) (prime-factors 2147483647)))
(define-test "prime-factors-fermat-5" (assert-equal '(3 715827883) (prime-factors 2147483649)))
(define-test "prime-factors-fermat-fix" (assert-equal '(641 6700417) (prime-factors 4294967297)))
(define-test "prime-factors-negative-composite" (assert-equal '(2 2 3) (prime-factors -12)))
(define-test "prime-factors-negative-prime" (assert-equal '(17) (prime-factors -17)))
;(define-test "prime-factors-error-type" (assert-error? (prime-factors "12")))
;(define-test "prime-factors-error-type-rational" (assert-error? (prime-factors (/ 12 1)))) ; Needs exact integer
;(define-test "prime-factors-error-arity-0" (assert-error? (prime-factors)))
;(define-test "prime-factors-error-arity-2" (assert-error? (prime-factors 12 13)))

(define-test "next-prime-from-2" (assert-equal 3 (next-prime 2)))
(define-test "next-prime-from-3" (assert-equal 5 (next-prime 3)))
(define-test "next-prime-from-4" (assert-equal 5 (next-prime 4)))
(define-test "next-prime-from-10" (assert-equal 11 (next-prime 10)))
(define-test "next-prime-from-11" (assert-equal 13 (next-prime 11)))
(define-test "next-prime-from-0" (assert-equal 2 (next-prime 0)))
(define-test "next-prime-from-1" (assert-equal 2 (next-prime 1)))
(define-test "next-prime-from-neg-5" (assert-equal 2 (next-prime -5)))
(define-test "next-prime-from-large-prime" (assert-equal 7927 (next-prime 7919))) ; 7927 is prime
(define-test "next-prime-from-large-composite" (assert-equal 7927 (next-prime 7921)))
(define-test "next-prime-from-big"
    (let ((p1 (expt 2 61))) ; A large number
      (assert-equal 2305843009213693951 (next-prime (- p1 2))))) ; Next prime after 2^61-1 (which is prime)
;(define-test "next-prime-error-type" (assert-error? (next-prime "10")))
;(define-test "next-prime-error-type-rational" (assert-error? (next-prime (/ 21 2))))
;(define-test "next-prime-error-arity-0" (assert-error? (next-prime)))
;(define-test "next-prime-error-arity-2" (assert-error? (next-prime 10 11)))
