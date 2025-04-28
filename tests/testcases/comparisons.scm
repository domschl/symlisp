;;; Tests for Comparison Functions

;; --- = (Numeric Equality) ---
(define-test "=-integers-equal" (assert-equal (= 5 5) #t))
(define-test "=-integers-unequal" (assert-equal (= 5 6) #f))
(define-test "=-negatives-equal" (assert-equal (= -3 -3) #t))
(define-test "=-mixed-sign-unequal" (assert-equal (= 5 -5) #f))
(define-test "=-zero" (assert-equal (= 0 0) #t))
(define-test "=-fraction-integer-equal" (assert-equal (= 4/2 2) #t))
(define-test "=-fractions-equal" (assert-equal (= 1/2 2/4) #t))
(define-test "=-fractions-unequal" (assert-equal (= 1/2 1/3) #f))
(define-test "=-fraction-integer-unequal" (assert-equal (= 1/2 1) #f))

;; --- > (Greater Than) ---
(define-test ">-integers-true" (assert-equal (> 6 5) #t))
(define-test ">-integers-false" (assert-equal (> 5 6) #f))
(define-test ">-integers-equal-false" (assert-equal (> 5 5) #f))
(define-test ">-negatives-true" (assert-equal (> -3 -4) #t))
(define-test ">-negatives-false" (assert-equal (> -4 -3) #f))
(define-test ">-mixed-sign-true" (assert-equal (> 1 -1) #t))
(define-test ">-mixed-sign-false" (assert-equal (> -1 1) #f))
(define-test ">-fraction-integer-true" (assert-equal (> 3/2 1) #t))
(define-test ">-fraction-integer-false" (assert-equal (> 1/2 1) #f))
(define-test ">-fractions-true" (assert-equal (> 1/2 1/3) #t))
(define-test ">-fractions-false" (assert-equal (> 1/3 1/2) #f))

;; --- < (Less Than) ---
(define-test "<-integers-true" (assert-equal (< 5 6) #t))
(define-test "<-integers-false" (assert-equal (< 6 5) #f))
(define-test "<-integers-equal-false" (assert-equal (< 5 5) #f))
(define-test "<-negatives-true" (assert-equal (< -4 -3) #t))
(define-test "<-negatives-false" (assert-equal (< -3 -4) #f))
(define-test "<-mixed-sign-true" (assert-equal (< -1 1) #t))
(define-test "<-mixed-sign-false" (assert-equal (< 1 -1) #f))
(define-test "<-fraction-integer-true" (assert-equal (< 1/2 1) #t))
(define-test "<-fraction-integer-false" (assert-equal (< 3/2 1) #f))
(define-test "<-fractions-true" (assert-equal (< 1/3 1/2) #t))
(define-test "<-fractions-false" (assert-equal (< 1/2 1/3) #f))

;; --- >= (Greater Than or Equal) ---
(define-test ">=-integers-true-gt" (assert-equal (>= 6 5) #t))
(define-test ">=-integers-true-eq" (assert-equal (>= 5 5) #t))
(define-test ">=-integers-false" (assert-equal (>= 5 6) #f))
(define-test ">=-fraction-integer-true-eq" (assert-equal (>= 4/2 2) #t))
(define-test ">=-fractions-true-gt" (assert-equal (>= 1/2 1/3) #t))

;; --- <= (Less Than or Equal) ---
(define-test "<=-integers-true-lt" (assert-equal (<= 5 6) #t))
(define-test "<=-integers-true-eq" (assert-equal (<= 5 5) #t))
(define-test "<=-integers-false" (assert-equal (<= 6 5) #f))
(define-test "<=-fraction-integer-true-eq" (assert-equal (<= 4/2 2) #t))
(define-test "<=-fractions-true-lt" (assert-equal (<= 1/3 1/2) #t))

;; --- EQ? (Identity / Pointer Equality) ---
(define test-eq-sym 'hello)
(define-test "eq?-same-symbol" (assert-equal (eq? test-eq-sym 'hello) #f)) ; Separate allocations without interning
(define-test "eq?-diff-symbol" (assert-equal (eq? 'hello 'world) #f))
(define-test "eq?-same-number" (assert-equal (eq? 5 5) #f)) ; Numbers are not eq?  TODO? ; Separate allocations without interning/optimization
(define-test "eq?-diff-number" (assert-equal (eq? 5 6) #f))
(define-test "eq?-same-boolean" (assert-equal (eq? #t #t) #t))
(define-test "eq?-diff-boolean" (assert-equal (eq? #t #f) #f))
(define-test "eq?-nil" (assert-equal (eq? '() '()) #t))
(define test-eq-list '(1 2))
(define-test "eq?-same-list-var" (assert-equal (eq? test-eq-list test-eq-list) #t))
(define-test "eq?-structurally-equal-lists" (assert-equal (eq? '(1 2) '(1 2)) #f)) ; Different allocations
(define test-eq-func (lambda () 1))
(define-test "eq?-same-func-var" (assert-equal (eq? test-eq-func test-eq-func) #t))
(define-test "eq?-diff-func-def" (assert-equal (eq? (lambda () 1) (lambda () 1)) #f))

;; --- EQUAL? (Structural Equality) ---
(define-test "equal?-same-symbol" (assert-equal (equal? 'hello 'hello) #t))
(define-test "equal?-diff-symbol" (assert-equal (equal? 'hello 'world) #f))
(define-test "equal?-same-number" (assert-equal (equal? 5 5) #t))
(define-test "equal?-diff-number" (assert-equal (equal? 5 6) #f))
(define-test "equal?-same-boolean" (assert-equal (equal? #t #t) #t))
(define-test "equal?-diff-boolean" (assert-equal (equal? #t #f) #f))
(define-test "equal?-nil" (assert-equal (equal? '() '()) #t))
(define-test "equal?-structurally-equal-lists" (assert-equal (equal? '(1 2) '(1 2)) #t))
(define-test "equal?-diff-lists" (assert-equal (equal? '(1 2) '(1 3)) #f))
(define-test "equal?-nested-lists-equal" (assert-equal (equal? '(1 (2 3)) '(1 (2 3))) #t))
(define-test "equal?-nested-lists-diff" (assert-equal (equal? '(1 (2 3)) '(1 (2 4))) #f))
(define-test "equal?-diff-types" (assert-equal (equal? 1 "1") #f))
(define-test "equal?-same-string" (assert-equal (equal? "abc" "abc") #t))
(define-test "equal?-diff-string" (assert-equal (equal? "abc" "def") #f))
(define test-equal-func (lambda () 1))
(define-test "equal?-same-func-var" (assert-equal (equal? test-equal-func test-equal-func) #t))
(define-test "equal?-diff-func-def" (assert-equal (equal? (lambda () 1) (lambda () 1)) #f)) ; Functions equal only if eq?