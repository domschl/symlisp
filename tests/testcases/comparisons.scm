;;; Tests for Comparison Functions

;; --- = (Numeric Equality) ---
(define-test-thunked "=-integers-equal" (lambda () (assert-equal (= 5 5) #t)))
(define-test-thunked "=-integers-unequal" (lambda () (assert-equal (= 5 6) #f)))
(define-test-thunked "=-negatives-equal" (lambda () (assert-equal (= -3 -3) #t)))
(define-test-thunked "=-mixed-sign-unequal" (lambda () (assert-equal (= 5 -5) #f)))
(define-test-thunked "=-zero" (lambda () (assert-equal (= 0 0) #t)))
(define-test-thunked "=-fraction-integer-equal" (lambda () (assert-equal (= 4/2 2) #t)))
(define-test-thunked "=-fractions-equal" (lambda () (assert-equal (= 1/2 2/4) #t)))
(define-test-thunked "=-fractions-unequal" (lambda () (assert-equal (= 1/2 1/3) #f)))
(define-test-thunked "=-fraction-integer-unequal" (lambda () (assert-equal (= 1/2 1) #f)))

;; --- > (Greater Than) ---
(define-test-thunked ">-integers-true" (lambda () (assert-equal (> 6 5) #t)))
(define-test-thunked ">-integers-false" (lambda () (assert-equal (> 5 6) #f)))
(define-test-thunked ">-integers-equal-false" (lambda () (assert-equal (> 5 5) #f)))
(define-test-thunked ">-negatives-true" (lambda () (assert-equal (> -3 -4) #t)))
(define-test-thunked ">-negatives-false" (lambda () (assert-equal (> -4 -3) #f)))
(define-test-thunked ">-mixed-sign-true" (lambda () (assert-equal (> 1 -1) #t)))
(define-test-thunked ">-mixed-sign-false" (lambda () (assert-equal (> -1 1) #f)))
(define-test-thunked ">-fraction-integer-true" (lambda () (assert-equal (> 3/2 1) #t)))
(define-test-thunked ">-fraction-integer-false" (lambda () (assert-equal (> 1/2 1) #f)))
(define-test-thunked ">-fractions-true" (lambda () (assert-equal (> 1/2 1/3) #t)))
(define-test-thunked ">-fractions-false" (lambda () (assert-equal (> 1/3 1/2) #f)))

;; --- < (Less Than) ---
(define-test-thunked "<-integers-true" (lambda () (assert-equal (< 5 6) #t)))
(define-test-thunked "<-integers-false" (lambda () (assert-equal (< 6 5) #f)))
(define-test-thunked "<-integers-equal-false" (lambda () (assert-equal (< 5 5) #f)))
(define-test-thunked "<-negatives-true" (lambda () (assert-equal (< -4 -3) #t)))
(define-test-thunked "<-negatives-false" (lambda () (assert-equal (< -3 -4) #f)))
(define-test-thunked "<-mixed-sign-true" (lambda () (assert-equal (< -1 1) #t)))
(define-test-thunked "<-mixed-sign-false" (lambda () (assert-equal (< 1 -1) #f)))
(define-test-thunked "<-fraction-integer-true" (lambda () (assert-equal (< 1/2 1) #t)))
(define-test-thunked "<-fraction-integer-false" (lambda () (assert-equal (< 3/2 1) #f)))
(define-test-thunked "<-fractions-true" (lambda () (assert-equal (< 1/3 1/2) #t)))
(define-test-thunked "<-fractions-false" (lambda () (assert-equal (< 1/2 1/3) #f)))

;; --- >= (Greater Than or Equal) ---
(define-test-thunked ">=-integers-true-gt" (lambda () (assert-equal (>= 6 5) #t)))
(define-test-thunked ">=-integers-true-eq" (lambda () (assert-equal (>= 5 5) #t)))
(define-test-thunked ">=-integers-false" (lambda () (assert-equal (>= 5 6) #f)))
(define-test-thunked ">=-fraction-integer-true-eq" (lambda () (assert-equal (>= 4/2 2) #t)))
(define-test-thunked ">=-fractions-true-gt" (lambda () (assert-equal (>= 1/2 1/3) #t)))

;; --- <= (Less Than or Equal) ---
(define-test-thunked "<=-integers-true-lt" (lambda () (assert-equal (<= 5 6) #t)))
(define-test-thunked "<=-integers-true-eq" (lambda () (assert-equal (<= 5 5) #t)))
(define-test-thunked "<=-integers-false" (lambda () (assert-equal (<= 6 5) #f)))
(define-test-thunked "<=-fraction-integer-true-eq" (lambda () (assert-equal (<= 4/2 2) #t)))
(define-test-thunked "<=-fractions-true-lt" (lambda () (assert-equal (<= 1/3 1/2) #t)))

;; --- EQ? (Identity / Pointer Equality) ---
(define test-eq-sym 'hello)
(define-test-thunked "eq?-same-symbol" (lambda () (assert-equal (eq? test-eq-sym 'hello) #t)))
(define-test-thunked "eq?-diff-symbol" (lambda () (assert-equal (eq? 'hello 'world) #f)))
(define-test-thunked "eq?-same-number" (lambda () (assert-equal (eq? 5 5) #f))) ; Numbers are not eq?  TODO? ; Separate allocations without interning/optimization
(define-test-thunked "eq?-diff-number" (lambda () (assert-equal (eq? 5 6) #f)))
(define-test-thunked "eq?-same-boolean" (lambda () (assert-equal (eq? #t #t) #t)))
(define-test-thunked "eq?-diff-boolean" (lambda () (assert-equal (eq? #t #f) #f)))
(define-test-thunked "eq?-nil" (lambda () (assert-equal (eq? '() '()) #t)))
(define test-eq-list '(1 2))
(define-test-thunked "eq?-same-list-var" (lambda () (assert-equal (eq? test-eq-list test-eq-list) #t)))
(define-test-thunked "eq?-structurally-equal-lists" (lambda () (assert-equal (eq? '(1 2) '(1 2)) #f))) ; Different allocations
(define test-eq-func (lambda () 1))
(define-test-thunked "eq?-same-func-var" (lambda () (assert-equal (eq? test-eq-func test-eq-func) #t)))
(define-test-thunked "eq?-diff-func-def" (lambda () (assert-equal (eq? (lambda () 1) (lambda () 1)) #f)))

;; --- EQUAL? (Structural Equality) ---
(define-test-thunked "equal?-same-symbol" (lambda () (assert-equal (equal? 'hello 'hello) #t)))
(define-test-thunked "equal?-diff-symbol" (lambda () (assert-equal (equal? 'hello 'world) #f)))
(define-test-thunked "equal?-same-number" (lambda () (assert-equal (equal? 5 5) #t)))
(define-test-thunked "equal?-diff-number" (lambda () (assert-equal (equal? 5 6) #f)))
(define-test-thunked "equal?-same-boolean" (lambda () (assert-equal (equal? #t #t) #t)))
(define-test-thunked "equal?-diff-boolean" (lambda () (assert-equal (equal? #t #f) #f)))
(define-test-thunked "equal?-nil" (lambda () (assert-equal (equal? '() '()) #t)))
(define-test-thunked "equal?-structurally-equal-lists" (lambda () (assert-equal (equal? '(1 2) '(1 2)) #t)))
(define-test-thunked "equal?-diff-lists" (lambda () (assert-equal (equal? '(1 2) '(1 3)) #f)))
(define-test-thunked "equal?-nested-lists-equal" (lambda () (assert-equal (equal? '(1 (2 3)) '(1 (2 3))) #t)))
(define-test-thunked "equal?-nested-lists-diff" (lambda () (assert-equal (equal? '(1 (2 3)) '(1 (2 4))) #f)))
(define-test-thunked "equal?-diff-types" (lambda () (assert-equal (equal? 1 "1") #f)))
(define-test-thunked "equal?-same-string" (lambda () (assert-equal (equal? "abc" "abc") #t)))
(define-test-thunked "equal?-diff-string" (lambda () (assert-equal (equal? "abc" "def") #f)))
(define test-equal-func (lambda () 1))
(define-test-thunked "equal?-same-func-var" (lambda () (assert-equal (equal? test-equal-func test-equal-func) #t)))
(define-test-thunked "equal?-diff-func-def" (lambda () (assert-equal (equal? (lambda () 1) (lambda () 1)) #f))) ; Functions equal only if eq?

;; --- NOT ---
(define-test-thunked "not-false" (lambda () (assert-equal (not #f) #t)))
(define-test-thunked "not-true" (lambda () (assert-equal (not #t) #f)))
(define-test-thunked "not-nil" (lambda () (assert-equal (not '()) #f))) ; NIL is truthy
(define-test-thunked "not-zero" (lambda () (assert-equal (not 0) #f))) ; 0 is truthy
(define-test-thunked "not-number" (lambda () (assert-equal (not 1) #f))) ; Numbers are truthy
(define-test-thunked "not-string" (lambda () (assert-equal (not "hello") #f))) ; Strings are truthy
(define-test-thunked "not-list" (lambda () (assert-equal (not (list 1 2)) #f))) ; Lists are truthy
(define-test-thunked "not-func" (lambda () (assert-equal (not (lambda () 1)) #f))) ; Functions are truthy
