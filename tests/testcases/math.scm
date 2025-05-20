;;;-----------------------------------------------------------------------------
;;; Number Theory Functions
;;;-----------------------------------------------------------------------------

;;; prime-factors Tests
(define-test-thunked "prime-factors-0" (lambda () (assert-equal '() (prime-factors 0))))
(define-test-thunked "prime-factors-1" (lambda () (assert-equal '() (prime-factors 1))))
(define-test-thunked "prime-factors-neg-1" (lambda () (assert-equal '() (prime-factors -1))))
(define-test-thunked "prime-factors-2" (lambda () (assert-equal '(2) (prime-factors 2))))
(define-test-thunked "prime-factors-3" (lambda () (assert-equal '(3) (prime-factors 3))))
(define-test-thunked "prime-factors-4" (lambda () (assert-equal '(2 2) (prime-factors 4))))
(define-test-thunked "prime-factors-6" (lambda () (assert-equal '(2 3) (prime-factors 6))))
(define-test-thunked "prime-factors-12" (lambda () (assert-equal '(2 2 3) (prime-factors 12))))
(define-test-thunked "prime-factors-17" (lambda () (assert-equal '(17) (prime-factors 17))))
(define-test-thunked "prime-factors-100" (lambda () (assert-equal '(2 2 5 5) (prime-factors 100))))
(define-test-thunked "prime-factors-99" (lambda () (assert-equal '(3 3 11) (prime-factors 99))))
(define-test-thunked "prime-factors-large-prime" (lambda () (assert-equal '(7919) (prime-factors 7919))))
(define-test-thunked "prime-factors-large-composite" (lambda () (assert-equal '(89 89) (prime-factors 7921)))) ; 89*89
(define-test-thunked "prime-factors-mersenne-31" (lambda () (assert-equal '(2147483647) (prime-factors 2147483647))))
(define-test-thunked "prime-factors-fermat-5" (lambda () (assert-equal '(3 715827883) (prime-factors 2147483649))))
(define-test-thunked "prime-factors-fermat-fix" (lambda () (assert-equal '(641 6700417) (prime-factors 4294967297))))
(define-test-thunked "prime-factors-negative-composite" (lambda () (assert-equal '(2 2 3) (prime-factors -12))))
(define-test-thunked "prime-factors-negative-prime" (lambda () (assert-equal '(17) (prime-factors -17))))
;(define-test-thunked "prime-factors-error-type" (lambda () (assert-error? (prime-factors "12"))))
;(define-test-thunked "prime-factors-error-type-rational" (lambda () (assert-error? (prime-factors (/ 12 1))))) ; Needs exact integer
;(define-test-thunked "prime-factors-error-arity-0" (assert-error? (prime-factors)))
;(define-test-thunked "prime-factors-error-arity-2" (assert-error? (prime-factors 12 13)))

(define-test-thunked "next-prime-from-2" (lambda () (assert-equal 3 (next-prime 2))))
(define-test-thunked "next-prime-from-3" (lambda () (assert-equal 5 (next-prime 3))))
(define-test-thunked "next-prime-from-4" (lambda () (assert-equal 5 (next-prime 4))))
(define-test-thunked "next-prime-from-10" (lambda () (assert-equal 11 (next-prime 10))))
(define-test-thunked "next-prime-from-11" (lambda () (assert-equal 13 (next-prime 11))))
(define-test-thunked "next-prime-from-0" (lambda () (assert-equal 2 (next-prime 0))))
(define-test-thunked "next-prime-from-1" (lambda () (assert-equal 2 (next-prime 1))))
(define-test-thunked "next-prime-from-neg-5" (lambda () (assert-equal 2 (next-prime -5))))
(define-test-thunked "next-prime-from-large-prime" (lambda () (assert-equal 7927 (next-prime 7919)))) ; 7927 is prime
(define-test-thunked "next-prime-from-large-composite" (lambda () (assert-equal 7927 (next-prime 7921))))
(define-test-thunked "next-prime-from-big"
    (lambda () (let ((p1 (expt 2 61))) ; A large number
      (assert-equal 2305843009213693951 (next-prime (- p1 2)))))) ; Next prime after 2^61-1 (which is prime)
;(define-test-thunked "next-prime-error-type" (lambda () (assert-error? (next-prime "10"))))
;(define-test-thunked "next-prime-error-type-rational" (lambda () (assert-error? (next-prime (/ 21 2)))))
;(define-test-thunked "next-prime-error-arity-0" (lambda () (assert-error? (next-prime))))
;(define-test-thunked "next-prime-error-arity-2" (lambda () (assert-error? (next-prime 10 11))))
