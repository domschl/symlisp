;;;-----------------------------------------------------------------------------
;;; Tests for Symbolic Algebra System Core Predicates and Accessors
;;;-----------------------------------------------------------------------------

;; Load the symbolics library (assuming it's in a loadable path or same directory for testing)
;; (load "symbolics.scm") ; This might be needed depending on your test runner setup

;;; --- Tests for Predicates ---

(define-test "symbolics-constant?-is-number-positive-integer"
  (assert-equal (constant? 5) #t))
(define-test "symbolics-constant?-is-number-zero"
  (assert-equal (constant? 0) #t))
(define-test "symbolics-constant?-is-number-negative-integer"
  (assert-equal (constant? -10) #t))
(define-test "symbolics-constant?-is-number-float"
  (assert-equal (constant? 3.14) #t))
(define-test "symbolics-constant?-is-number-rational"
  (assert-equal (constant? 1/2) #t))

(define-test "symbolics-constant?-is-not-symbol"
  (assert-equal (constant? 'x) #f))
(define-test "symbolics-constant?-is-not-list"
  (assert-equal (constant? '(+ 1 2)) #f))
(define-test "symbolics-constant?-is-not-string"
  (assert-equal (constant? "hello") #f))
(define-test "symbolics-constant?-is-not-boolean"
  (assert-equal (constant? #t) #f))

(define-test "symbolics-variable?-is-symbol-simple"
  (assert-equal (variable? 'x) #t))
(define-test "symbolics-variable?-is-symbol-greek"
  (assert-equal (variable? 'alpha) #t))
(define-test "symbolics-variable?-is-symbol-with-hyphen"
  (assert-equal (variable? 'my-var) #t))

(define-test "symbolics-variable?-is-not-number"
  (assert-equal (variable? 5) #f))
(define-test "symbolics-variable?-is-not-list"
  (assert-equal (variable? '(+ x 1)) #f))
(define-test "symbolics-variable?-is-not-operator-plus"
  (assert-equal (variable? '+) #f))
(define-test "symbolics-variable?-is-not-operator-sin"
  (assert-equal (variable? 'sin) #f))

(define-test "symbolics-atomic-expr?-is-number"
  (assert-equal (atomic-expr? 5) #t))
(define-test "symbolics-atomic-expr?-is-variable"
  (assert-equal (atomic-expr? 'x) #t))

(define-test "symbolics-atomic-expr?-is-not-list"
  (assert-equal (atomic-expr? '(+ 1 2)) #f))
(define-test "symbolics-atomic-expr?-is-not-operator"
  (assert-equal (atomic-expr? '+) #f))

(define-test "symbolics-compound-expr?-is-sum"
  (assert-equal (compound-expr? '(+ 1 x)) #t))
(define-test "symbolics-compound-expr?-is-product"
  (assert-equal (compound-expr? '(* a b c)) #t))
(define-test "symbolics-compound-expr?-is-negation"
  (assert-equal (compound-expr? '(- y)) #t))
(define-test "symbolics-compound-expr?-is-quotient"
  (assert-equal (compound-expr? '(/ 10 2)) #t))
(define-test "symbolics-compound-expr?-is-power"
  (assert-equal (compound-expr? '(^ x 2)) #t))
(define-test "symbolics-compound-expr?-is-sin"
  (assert-equal (compound-expr? '(sin x)) #t))
(define-test "symbolics-compound-expr?-is-operator-no-args"
  (assert-equal (compound-expr? '(+)) #t))


(define-test "symbolics-compound-expr?-is-not-number"
  (assert-equal (compound-expr? 5) #f))
(define-test "symbolics-compound-expr?-is-not-variable"
  (assert-equal (compound-expr? 'x) #f))
(define-test "symbolics-compound-expr?-is-not-unknown-operator"
  (assert-equal (compound-expr? '(foo x y)) #f))
(define-test "symbolics-compound-expr?-is-not-empty-list"
  (assert-equal (compound-expr? '()) #f))
(define-test "symbolics-compound-expr?-is-not-list-starting-with-number"
  (assert-equal (compound-expr? '(1 + 2)) #f))

(define-test "symbolics-sum?-is-sum-binary"
  (assert-equal (sum? '(+ 1 x)) #t))
(define-test "symbolics-sum?-is-sum-nary"
  (assert-equal (sum? '(+ a b c d)) #t))
(define-test "symbolics-sum?-is-sum-no-args"
  (assert-equal (sum? '(+)) #t))

(define-test "symbolics-sum?-is-not-product"
  (assert-equal (sum? '(* 1 x)) #f))
(define-test "symbolics-sum?-is-not-unknown-op"
  (assert-equal (sum? '(plus 1 x)) #f))
(define-test "symbolics-sum?-is-not-number"
  (assert-equal (sum? 5) #f))

(define-test "symbolics-product?-is-product-binary"
  (assert-equal (product? '(* 1 x)) #t))
(define-test "symbolics-product?-is-product-nary"
  (assert-equal (product? '(* a b c d)) #t))
(define-test "symbolics-product?-is-product-no-args"
  (assert-equal (product? '(*)) #t))

(define-test "symbolics-product?-is-not-sum"
  (assert-equal (product? '(+ 1 x)) #f))
(define-test "symbolics-product?-is-not-unknown-op"
  (assert-equal (product? '(times 1 x)) #f))
(define-test "symbolics-product?-is-not-variable"
  (assert-equal (product? 'x) #f))

(define-test "symbolics-power?-is-power-var-num"
  (assert-equal (power? '(^ x 2)) #t))
(define-test "symbolics-power?-is-power-expr-var"
  (assert-equal (power? '(^ (+ a b) y)) #t))

(define-test "symbolics-power?-is-not-sum"
  (assert-equal (power? '(+ x 2)) #f))
(define-test "symbolics-power?-is-not-unary"
  (assert-equal (power? '(^ x)) #f))
(define-test "symbolics-power?-is-not-ternary"
  (assert-equal (power? '(^ x 2 3)) #f))
(define-test "symbolics-power?-is-not-unknown-op"
  (assert-equal (power? '(pow x 2)) #f))

(define-test "symbolics-negation?-is-negation-var"
  (assert-equal (negation? '(- x)) #t))
(define-test "symbolics-negation?-is-negation-expr"
  (assert-equal (negation? '(- (+ a b))) #t))

(define-test "symbolics-negation?-is-not-difference"
  (assert-equal (negation? '(- x y)) #f))
(define-test "symbolics-negation?-is-not-no-args"
  (assert-equal (negation? '(-)) #f))
(define-test "symbolics-negation?-is-not-sum"
  (assert-equal (negation? '(+ x)) #f))

(define-test "symbolics-difference?-is-difference-var-var"
  (assert-equal (difference? '(- x y)) #t))
(define-test "symbolics-difference?-is-difference-num-num"
  (assert-equal (difference? '(- 10 2)) #t))

(define-test "symbolics-difference?-is-not-negation"
  (assert-equal (difference? '(- x)) #f))
(define-test "symbolics-difference?-is-not-no-args"
  (assert-equal (difference? '(-)) #f))
(define-test "symbolics-difference?-is-not-ternary"
  (assert-equal (difference? '(- x y z)) #f))
(define-test "symbolics-difference?-is-not-sum"
  (assert-equal (difference? '(+ x y)) #f))

(define-test "symbolics-quotient?-is-quotient-var-var"
  (assert-equal (quotient? '(/ x y)) #t))
(define-test "symbolics-quotient?-is-quotient-num-num"
  (assert-equal (quotient? '(/ 10 2)) #t))

(define-test "symbolics-quotient?-is-not-unary"
  (assert-equal (quotient? '(/ x)) #f))
(define-test "symbolics-quotient?-is-not-no-args"
  (assert-equal (quotient? '(/)) #f))
(define-test "symbolics-quotient?-is-not-ternary"
  (assert-equal (quotient? '(/ x y z)) #f))
(define-test "symbolics-quotient?-is-not-product"
  (assert-equal (quotient? '(* x y)) #f))

;;; --- Tests for Accessors ---

(define-test "symbolics-operator-accessor-sum"
  (assert-equal (operator '(+ x 1)) '+))
(define-test "symbolics-operator-accessor-product"
  (assert-equal (operator '(* a b)) '*))
;; (define-test "symbolics-operator-accessor-error-var"
;;   (assert-error (operator 'x))) ; assert-error not supported yet
;; (define-test "symbolics-operator-accessor-error-num"
;;   (assert-error (operator 5)))

(define-test "symbolics-operands-accessor-sum"
  (assert-equal (operands '(+ x 1)) '(x 1)))
(define-test "symbolics-operands-accessor-product"
  (assert-equal (operands '(* a b c)) '(a b c)))
(define-test "symbolics-operands-accessor-sin"
  (assert-equal (operands '(sin x)) '(x)))
(define-test "symbolics-operands-accessor-sum-no-args"
  (assert-equal (operands '(+)) '()))
;; (define-test "symbolics-operands-accessor-error-var"
;;   (assert-error (operands 'x)))
;; (define-test "symbolics-operands-accessor-error-num"
;;   (assert-error (operands 5)))

(define-test "symbolics-terms-accessor-sum"
  (assert-equal (terms '(+ x 1 y)) '(x 1 y)))
(define-test "symbolics-terms-accessor-sum-no-args"
  (assert-equal (terms '(+)) '()))
;; (define-test "symbolics-terms-accessor-error-product"
;;   (assert-error (terms '(* x y))))

(define-test "symbolics-factors-accessor-product"
  (assert-equal (factors '(* x 2 z)) '(x 2 z)))
(define-test "symbolics-factors-accessor-product-no-args"
  (assert-equal (factors '(*)) '()))
;; (define-test "symbolics-factors-accessor-error-sum"
;;   (assert-error (factors '(+ x y))))

(define-test "symbolics-base-accessor-simple-power"
  (assert-equal (base '(^ x 2)) 'x))
(define-test "symbolics-exponent-accessor-simple-power"
  (assert-equal (exponent '(^ x 2)) 2))
(define-test "symbolics-base-accessor-complex-power"
  (assert-equal (base '(^ (+ a 1) (* b 2))) '(+ a 1)))
(define-test "symbolics-exponent-accessor-complex-power"
  (assert-equal (exponent '(^ (+ a 1) (* b 2))) '(* b 2)))
;; (define-test "symbolics-base-accessor-error-sum"
;;   (assert-error (base '(+ x 2))))
;; (define-test "symbolics-exponent-accessor-error-unary-power"
;;   (assert-error (exponent '(^ x))))

(define-test "symbolics-negated-expr-accessor-var"
  (assert-equal (negated-expr '(- x)) 'x))
(define-test "symbolics-negated-expr-accessor-expr"
  (assert-equal (negated-expr '(- (* a b))) '(* a b)))
;; (define-test "symbolics-negated-expr-accessor-error-difference"
;;   (assert-error (negated-expr '(- x y))))

(define-test "symbolics-minuend-accessor-simple"
  (assert-equal (minuend '(- x y)) 'x))
(define-test "symbolics-subtrahend-accessor-simple"
  (assert-equal (subtrahend '(- x y)) 'y))
(define-test "symbolics-minuend-accessor-complex"
  (assert-equal (minuend '(- 10 (+ a b))) 10))
(define-test "symbolics-subtrahend-accessor-complex"
  (assert-equal (subtrahend '(- 10 (+ a b))) '(+ a b)))
;; (define-test "symbolics-minuend-accessor-error-negation"
;;   (assert-error (minuend '(- x))))
;; (define-test "symbolics-subtrahend-accessor-error-negation"
;;   (assert-error (subtrahend '(- x))))

(define-test "symbolics-numerator-accessor-simple"
  (assert-equal (quotient-numerator '(/ x y)) 'x))
(define-test "symbolics-denominator-accessor-simple"
  (assert-equal (quotient-denominator '(/ x y)) 'y))
(define-test "symbolics-numerator-accessor-complex"
  (assert-equal (quotient-numerator '(/ (* a 2) (+ c d))) '(* a 2)))
(define-test "symbolics-denominator-accessor-complex"
  (assert-equal (quotient-denominator '(/ (* a 2) (+ c d))) '(+ c d)))
;; (define-test "symbolics-numerator-accessor-error-unary"
;;   (assert-error (numerator '(/ x))))
;; (define-test "symbolics-denominator-accessor-error-unary"
;;   (assert-error (denominator '(/ x))))

;; Add more tests as new predicates/accessors for other functions (sin, cos, etc.) are added.
