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

;;; --- Tests for Simplify Function ---

;; Atomic Expressions
(define-test "simplify-atomic-constant"
  (assert-equal (simplify 5) 5))
(define-test "simplify-atomic-variable"
  (assert-equal (simplify 'x) 'x))

;; Constant Folding
(define-test "simplify-constant-folding-sum"
  (assert-equal (simplify '(+ 1 2 3)) 6))
(define-test "simplify-constant-folding-product"
  (assert-equal (simplify '(* 2 3 4)) 24))
(define-test "simplify-constant-folding-difference"
  (assert-equal (simplify '(- 10 3)) 7))
(define-test "simplify-constant-folding-quotient"
  (assert-equal (simplify '(/ 20 4)) 5))
(define-test "simplify-constant-folding-power"
  (assert-equal (simplify '(^ 2 3)) 8))
(define-test "simplify-constant-folding-negation"
  (assert-equal (simplify '(- 5)) -5))

;; Arithmetic Identities - Sum
(define-test "simplify-sum-add-zero-right"
  (assert-equal (simplify '(+ x 0)) 'x))
(define-test "simplify-sum-add-zero-left"
  (assert-equal (simplify '(+ 0 x)) 'x))
(define-test "simplify-sum-add-zero-middle"
  (assert-equal (simplify '(+ x 0 y)) '(+ x y)))
(define-test "simplify-sum-multiple-zeros"
  (assert-equal (simplify '(+ 0 x 0 y 0)) '(+ x y)))
(define-test "simplify-sum-all-zeros"
  (assert-equal (simplify '(+ 0 0 0)) 0))
(define-test "simplify-sum-no-operands" ; make-sum handles this
  (assert-equal (simplify '(+)) 0))

;; Tests for Term Collection in Sums
(define-test "simplify-sum-collect-simple-variables"
  (assert-equal (simplify '(+ x y x)) '(+ y (* 2 x)))) ; Assuming y < x for term<? string fallback
(define-test "simplify-sum-collect-variables-with-constants"
  (assert-equal (simplify '(+ x 2 y 3 x 1)) '(+ 6 y (* 2 x))))
(define-test "simplify-sum-collect-and-cancel-variables"
  (assert-equal (simplify '(+ x y (- x))) 'y))
(define-test "simplify-sum-collect-and-cancel-multiple-variables"
  (assert-equal (simplify '(+ a b (- a) c (- b))) 'c))
(define-test "simplify-sum-collect-terms-with-coefficients"
  (assert-equal (simplify '(+ (* 2 x) y (* 3 x))) '(+ y (* 5 x))))
(define-test "simplify-sum-collect-terms-with-negative-coefficients"
  (assert-equal (simplify '(+ (* 5 x) y (* -2 x))) '(+ y (* 3 x))))
(define-test "simplify-sum-collect-and-cancel-terms-with-coefficients"
  (assert-equal (simplify '(+ (* 2 x) y (* -2 x))) 'y))
(define-test "simplify-sum-collect-complex-terms"
  ;; (+ (^ x 2) (* 3 (^ x 2)) y) -> (+ y (* 4 (^ x 2)))
  (assert-equal (simplify '(+ (^ x 2) (* 3 (^ x 2)) y)) '(+ y (* 4 (^ x 2)))))
(define-test "simplify-sum-collect-from-readme-example"
  ;; (simplify '(+ x (* -1 y) (* 2 x) y)) => (* 3 x)
  (assert-equal (simplify '(+ x (* -1 y) (* 2 x) y)) '(* 3 x)))
(define-test "simplify-sum-collect-all-cancel-to-zero"
  (assert-equal (simplify '(+ x y (- x) (- y))) 0))
(define-test "simplify-sum-collect-with-constants-cancelling-variables"
  (assert-equal (simplify '(+ 5 x y (- x) (- y) 2)) 7))
(define-test "simplify-sum-collect-nested-and-flattened"
  ;; (+ x (+ y (* 2 x)) (- y) z) -> (+ x y (* 2 x) (- y) z) -> (+ z (* 3 x))
  (assert-equal (simplify '(+ x (+ y (* 2 x)) (- y) z)) '(+ z (* 3 x))))
(define-test "simplify-sum-collect-with-negation-terms"
  ;; (+ a (- b) (* 2 a) b) -> (+ b (- b) (* 3 a)) -> (* 3 a)
  (assert-equal (simplify '(+ a (- b) (* 2 a) b)) '(* 3 a)))
(define-test "simplify-sum-collect-leading-to-single-negative-term"
  (assert-equal (simplify '(+ (* 2 x) (* -3 x))) '(- x)))
(define-test "simplify-sum-collect-leading-to-single-constant"
  (assert-equal (simplify '(+ x 5 (- x))) 5))

;; Arithmetic Identities - Product
(define-test "simplify-product-mult-one-right"
  (assert-equal (simplify '(* x 1)) 'x))
(define-test "simplify-product-mult-one-left"
  (assert-equal (simplify '(* 1 x)) 'x))
(define-test "simplify-product-mult-one-middle"
  (assert-equal (simplify '(* x 1 y)) '(* x y)))
(define-test "simplify-product-multiple-ones"
  (assert-equal (simplify '(* 1 x 1 y 1)) '(* x y)))
(define-test "simplify-product-all-ones"
  (assert-equal (simplify '(* 1 1 1)) 1))
(define-test "simplify-product-mult-zero-right"
  (assert-equal (simplify '(* x 0)) 0))
(define-test "simplify-product-mult-zero-left"
  (assert-equal (simplify '(* 0 x)) 0))
(define-test "simplify-product-mult-zero-middle"
  (assert-equal (simplify '(* x 0 y)) 0))
(define-test "simplify-product-no-operands" ; make-product handles this
  (assert-equal (simplify '(*)) 1))

;; Arithmetic Identities - Power
(define-test "simplify-power-exponent-one"
  (assert-equal (simplify '(^ x 1)) 'x))
(define-test "simplify-power-exponent-zero"
  (assert-equal (simplify '(^ x 0)) 1))
(define-test "simplify-power-base-one"
  (assert-equal (simplify '(^ 1 x)) 1))
(define-test "simplify-power-base-zero-positive-exponent" ; 0^x = 0 for x > 0
  (assert-equal (simplify '(^ 0 2)) 0))
(define-test "simplify-power-base-zero-variable-exponent" 
  (assert-equal (simplify '(^ 0 x)) '(^ 0 x)))
(define-test "simplify-power-zero-to-zero" ; 0^0 = 1
  (assert-equal (simplify '(^ 0 0)) 1))


;; Arithmetic Identities - Negation
(define-test "simplify-negation-of-zero"
  (assert-equal (simplify '(- 0)) 0))
(define-test "simplify-negation-double"
  (assert-equal (simplify '(- (- x))) 'x))
(define-test "simplify-negation-of-product-with-constant"
  (assert-equal (simplify '(- (* 2 x))) '(* -2 x)))
(define-test "simplify-negation-of-product-with-negative-constant"
  (assert-equal (simplify '(- (* -2 x))) '(* 2 x)))

;; Arithmetic Identities - Difference
(define-test "simplify-difference-subtract-zero"
  (assert-equal (simplify '(- x 0)) 'x))
(define-test "simplify-difference-zero-subtract-x" ; -> (- x)
  (assert-equal (simplify '(- 0 x)) '(- x)))
(define-test "simplify-difference-subtract-self"
  (assert-equal (simplify '(- x x)) 0))
(define-test "simplify-difference-canonical-form" ; (- a b) -> (+ a (* -1 b)) -> (+ a (- b))
  (assert-equal (simplify '(- a b)) '(+ a (- b))))
(define-test "simplify-difference-constants-to-canonical"
  (assert-equal (simplify '(- 2 5)) -3)) ; Should fold, but if not, canonical is (+ 2 (- 5))

;; Arithmetic Identities - Quotient
(define-test "simplify-quotient-divide-by-one"
  (assert-equal (simplify '(/ x 1)) 'x))
(define-test "simplify-quotient-zero-divided-by-x"
  (assert-equal (simplify '(/ 0 x)) 0))
(define-test "simplify-quotient-divide-self" ; x/x = 1 (assuming x != 0)
  (assert-equal (simplify '(/ x x)) 1))
(define-test "simplify-quotient-divide-by-zero-remains" ; Or error, current returns unsimplified
  (assert-equal (simplify '(/ x 0)) '(/ x 0)))

;;; --- Tests for term<? canonical ordering ---
(define-test "term<?-constants-1"
  (assert-true (term<? 1 2)))
(define-test "term<?-constants-2"
  (assert-false (term<? 2 1)))
(define-test "term<?-constants-3"
  (assert-false (term<? 1 1)))

(define-test "term<?-constant-vs-variable-1"
  (assert-true (term<? 1 'x)))
(define-test "term<?-constant-vs-variable-2"
  (assert-false (term<? 'x 1)))

(define-test "term<?-constant-vs-compound-1"
  (assert-true (term<? 1 '(+ x 1))))
(define-test "term<?-constant-vs-compound-2"
  (assert-false (term<? '(+ x 1) 1)))

(define-test "term<?-variables-1"
  (assert-true (term<? 'a 'b)))
(define-test "term<?-variables-2"
  (assert-false (term<? 'b 'a)))
(define-test "term<?-variables-3"
  (assert-false (term<? 'a 'a)))
(define-test "term<?-variables-4"
  (assert-true (term<? 'x 'y)))  ; Assuming standard alphabetical
(define-test "term<?-variables-5"
  (assert-true (term<? 'y 'z))) ; Assuming standard alphabetical

(define-test "term<?-variable-vs-compound-1"
  (assert-true (term<? 'x '(+ x 1))))  ; Variable < Compound
(define-test "term<?-variable-vs-compound-2"
  (assert-false (term<? '(+ x 1) 'x))) ; Compound not < Variable

;; Recursive Simplification
(define-test "simplify-recursive-sum-product"
  (assert-equal (simplify '(+ (* 2 3) x)) '(+ 6 x)))
(define-test "simplify-recursive-product-sum"
  (assert-equal (simplify '(* (+ 1 2) y)) '(* 3 y)))
(define-test "simplify-recursive-power-sum-diff"
  (assert-equal (simplify '(^ (+ 1 1) (- 5 2))) 8)) ; (^ 2 3) -> 8
(define-test "simplify-recursive-sum-of-zeros"
  (assert-equal (simplify '(+ (* x 0) (* 0 y))) 0))
(define-test "simplify-recursive-product-of-ones"
  (assert-equal (simplify '(* (^ x 0) (^ y 0))) 1))

;; Combination of Rules
(define-test "simplify-combo-sum-prod-identities"
  (assert-equal (simplify '(+ (* x 1) (* y 0) z)) '(+ x z)))
(define-test "simplify-combo-diff-prod-negation" ; (- (* 5 x) (- 0 y)) -> (+ (* 5 x) (- (- y))) -> (+ (* 5 x) y)
  (assert-equal (simplify '(- (* 5 x) (- 0 y))) '(+ y (* 5 x))))
(define-test "simplify-combo-quotient-power"
  (assert-equal (simplify '(/ (^ x 1) (^ y 0))) 'x)) ; (/ x 1) -> x

;; Expressions that are already simple or cannot be simplified further by current rules
(define-test "simplify-already-simple-sum"
  (assert-equal (simplify '(+ x y)) '(+ x y)))
(define-test "simplify-already-simple-product"
  (assert-equal (simplify '(* x y)) '(* x y)))
(define-test "simplify-already-simple-power"
  (assert-equal (simplify '(^ x y)) '(^ x y)))
(define-test "simplify-unknown-function-simplifies-args"
  (assert-equal (simplify '(foo (+ 1 0) (* 2 3))) '(foo 1 6))) 

(define-test "simplify-unknown-function-simplifies-args-example2"
  (assert-equal (simplify '(sin (+ x 0))) '(sin x)))


;; Test canonicalization of difference via sum
(define-test "simplify-difference-complex-to-canonical"
  (assert-equal (simplify '(- (+ x 1) (+ y 2))) '(+ -1 x (- y)))) ; Adapted expectation

;; Test product with -1 constant factor
(define-test "simplify-product-with-minus-one-constant"
  (assert-equal (simplify '(* -1 x y)) '(- (* x y))))
(define-test "simplify-product-with-constants-evaluating-to-minus-one"
  (assert-equal (simplify '(* 1 -1 x y)) '(- (* x y))))

;; Test sum with constants and non-constants
(define-test "simplify-sum-constants-and-non-constants"
  (assert-equal (simplify '(+ 2 x 3 y 4)) '(+ 9 x y)))
(define-test "simplify-sum-only-non-constants"
  (assert-equal (simplify '(+ x y z)) '(+ x y z)))
(define-test "simplify-sum-one-non-constant-and-zero"
  (assert-equal (simplify '(+ x 0)) 'x))
(define-test "simplify-sum-one-non-constant-and-constants"
  (assert-equal (simplify '(+ 2 x 3)) '(+ 5 x)))

;; Test product with constants and non-constants
(define-test "simplify-product-constants-and-non-constants"
  (assert-equal (simplify '(* 2 x 3 y 4)) '(* 24 x y)))
(define-test "simplify-product-only-non-constants"
  (assert-equal (simplify '(* x y z)) '(* x y z)))
(define-test "simplify-product-one-non-constant-and-one"
  (assert-equal (simplify '(* x 1)) 'x))
(define-test "simplify-product-one-non-constant-and-constants"
  (assert-equal (simplify '(* 2 x 3)) '(* 6 x)))

;; Test for (- 0 x) -> (- x)
(define-test "simplify-zero-minus-variable-is-negation"
  (assert-equal (simplify '(- 0 var)) '(- var)))

;; Test for (- (- x)) -> x in various contexts
(define-test "simplify-sum-with-double-negation"
  (assert-equal (simplify '(+ a (- (- b)))) '(+ a b)))

;; Test for (- (* c x)) -> (* (- c) x)
(define-test "simplify-negation-of-product-positive-const"
  (assert-equal (simplify '(- (* 5 k))) '(* -5 k)))
(define-test "simplify-negation-of-product-negative-const"
  (assert-equal (simplify '(- (* -5 k))) '(* 5 k)))

;; Test for simplify-difference converting to (+ a (* -1 b)) and then simplifying that
(define-test "simplify-diff-a-minus-const-b"
  (assert-equal (simplify '(- a 5)) '(+ -5 a))) ; Adapted expectation
(define-test "simplify-diff-const-a-minus-b"
  (assert-equal (simplify '(- 5 b)) '(+ 5 (- b))))
(define-test "simplify-diff-a-minus-neg-b" ; (- a (- b)) -> (+ a (- (* -1 (- b)))) -> (+ a (- (- b))) -> (+ a b)
  (assert-equal (simplify '(- a (- b))) '(+ a b)))

;;; --- Tests for Canonical Forms: Flattening and Ordering ---

;; Flattening Sums
(define-test "simplify-flatten-sum-simple"
  (assert-equal (simplify '(+ x (+ y z))) '(+ x y z))) ; Assuming x < y < z
(define-test "simplify-flatten-sum-nested-right"
  (assert-equal (simplify '(+ a (+ b (+ c d)))) '(+ a b c d))) ; Assuming a < b < c < d
(define-test "simplify-flatten-sum-nested-left"
  (assert-equal (simplify '(+ (+ (+ a b) c) d)) '(+ a b c d)))
(define-test "simplify-flatten-sum-with-constants"
  (assert-equal (simplify '(+ 1 (+ x 2 (+ y 3)))) '(+ 6 x y))) ; Constants collected, then sorted with vars
(define-test "simplify-flatten-sum-identities-involved"
  (assert-equal (simplify '(+ x (+ 0 y (+ z 0)))) '(+ x y z)))

;; Flattening Products
(define-test "simplify-flatten-product-simple"
  (assert-equal (simplify '(* x (* y z))) '(* x y z))) ; Assuming x < y < z
(define-test "simplify-flatten-product-nested-right"
  (assert-equal (simplify '(* a (* b (* c d)))) '(* a b c d))) ; Assuming a < b < c < d
(define-test "simplify-flatten-product-nested-left"
  (assert-equal (simplify '(* (* (* a b) c) d)) '(* a b c d)))
(define-test "simplify-flatten-product-with-constants"
  (assert-equal (simplify '(* 2 (* x 3 (* y 4)))) '(* 24 x y))) ; Constants collected, then sorted
(define-test "simplify-flatten-product-identities-involved"
  (assert-equal (simplify '(* x (* 1 y (* z 1)))) '(* x y z)))
(define-test "simplify-flatten-product-zero-involved"
  (assert-equal (simplify '(* x (* 0 y (* z 1)))) 0))

;; Ordering Terms in Sums (relies on term<?)
(define-test "simplify-order-sum-vars"
  (assert-equal (simplify '(+ z y x)) '(+ x y z)))
(define-test "simplify-order-sum-const-vars"
  (assert-equal (simplify '(+ y 10 x 5)) '(+ 15 x y)))
(define-test "simplify-order-sum-const-vars-compounds"
  (assert-equal (simplify '(+ (foo b) z 20 y 10 x (bar a)))
                '(+ 30 x y z (bar a) (foo b)))) ; Order of (bar a) (foo b) depends on object->string

;; Ordering Factors in Products (relies on term<?)
(define-test "simplify-order-product-vars"
  (assert-equal (simplify '(* z y x)) '(* x y z)))
(define-test "simplify-order-product-const-vars"
  (assert-equal (simplify '(* y 10 x 5)) '(* 50 x y)))
(define-test "simplify-order-product-const-vars-compounds"
  (assert-equal (simplify '(* (foo b) z 20 y 10 x (bar a)))
                '(* 200 x y z (bar a) (foo b)))) ; Order of (bar a) (foo b) depends on object->string

;;; --- Tests for Power-Grouping in Products (via simplify-product) ---
(define-test "simplify-product-group-single-variable-pair"
  (assert-equal (simplify '(* x x)) '(^ x 2)))
(define-test "simplify-product-group-single-variable-triple"
  (assert-equal (simplify '(* y y y)) '(^ y 3)))
(define-test "simplify-product-group-with-other-factors"
  (assert-equal (simplify '(* a x x b x c)) '(* a b c (^ x 3)))) ; Assumes a,b,c sorted before x
(define-test "simplify-product-group-multiple-variables"
  (assert-equal (simplify '(* a b a b a)) '(* (^ a 3) (^ b 2))))
(define-test "simplify-product-group-with-constants"
  (assert-equal (simplify '(* 2 x 3 x)) '(* 6 (^ x 2))))
(define-test "simplify-product-group-no-grouping-needed"
  (assert-equal (simplify '(* a b c)) '(* a b c)))
(define-test "simplify-product-group-as-power"
  (assert-equal (simplify '(* (^ x 2) x)) '(^ x 3)))
(define-test "simplify-product-group-complex-factors"
  ;; Input: (* (+ a 1) x (+ a 1) y (+ a 1))
  ;; simplify on (+ a 1) -> (+ 1 a)
  ;; Sorted factors before grouping: (x y (+ 1 a) (+ 1 a) (+ 1 a))
  ;; Grouped: (x y (^ (+ 1 a) 3))
  (assert-equal (simplify '(* (+ a 1) x (+ a 1) y (+ a 1)))
                '(* x y (^ (+ 1 a) 3)))) ; Adapted: (+ 1 a) due to inner simplify
(define-test "simplify-product-group-flatten-then-group"
  ;; Input: (* x (* y x y) z x) -> flattened to (* x y x y z x)
  ;; Sorted non-constant factors: (x x x y y z) (assuming alphabetical for vars)
  ;; Grouped: ((^ x 3) (^ y 2) z)
  ;; term<? sorts z (var) before (^ x 3) and (^ y 2) (compounds)
  ;; Then (^ x 3) vs (^ y 2) by object->string. Assume (^ x 3) < (^ y 2)
  (assert-equal (simplify '(* x (* y x y) z x)) '(* z (^ x 3) (^ y 2)))) ; Adapted based on term<?

;; Combined Flattening and Ordering
(define-test "simplify-flatten-order-sum-complex"
  (assert-equal (simplify '(+ c (+ 10 a) 5 (+ b x))) '(+ 15 a b c x)))
(define-test "simplify-flatten-order-product-complex"
  (assert-equal (simplify '(* c (* 10 a) 5 (* b x))) '(* 50 a b c x)))

;; Interaction with (- a b) -> (+ a (* -1 b))
(define-test "simplify-flatten-order-after-difference-conversion"
  (assert-equal (simplify '(- (+ z 3) (+ x 1))) '(+ 2 z (- x))))
(define-test "simplify-flatten-order-sum-with-negations"
  ;; (+ c (- b) a) -> (+ a (- b) c) (if a < c and (- b) is treated as compound after vars)
  ;; or (+ (- b) a c) (if (-b) comes before a and c due to term<?)
  ;; Assuming variables first, then compounds:
  (assert-equal (simplify '(+ c (- b) a)) '(+ a c (- b))))

;;; --- Tests for Expand Function ---

;; Distributive Property: (* a (+ b c ...)) -> (+ (* a b) (* a c) ...)
(define-test "expand-distribute-simple"
  (assert-equal (expand '(* a (+ b c))) '(+ (* a b) (* a c))))
(define-test "expand-distribute-constant-factor"
  (assert-equal (expand '(* 2 (+ x y))) '(+ (* 2 x) (* 2 y))))
(define-test "expand-distribute-nary-sum"
  (assert-equal (expand '(* a (+ b c d))) '(+ (* a b) (* a c) (* a d))))
(define-test "expand-distribute-sum-first" ; Should reorder due to simplify
  (assert-equal (expand '(* (+ b c) a)) '(+ (* a b) (* a c))))
(define-test "expand-distribute-multiple-factors"
  (assert-equal (expand '(* a x (+ b c) y)) '(+ (* a b x y) (* a c x y))))
(define-test "expand-distribute-no-sum"
  (assert-equal (expand '(* a b c)) '(* a b c)))
(define-test "expand-distribute-sum-with-one-term" ; simplify might turn (+ x) to x
  (assert-equal (expand '(* a (+ x))) '(* a x)))
(define-test "expand-distribute-product-of-sums"
  ;; (* (+ a b) (+ c d)) -> expand first sum: (+ (* (+ a b) c) (* (+ a b) d))
  ;; -> simplify: (+ (* c (+ a b)) (* d (+ a b)))
  ;; -> expand again: (+ (+ (* a c) (* b c)) (+ (* a d) (* b d)))
  ;; -> simplify: (+ (* a c) (* a d) (* b c) (* b d)) (order depends on term<?)
  (assert-equal (expand '(* (+ a b) (+ c d)))
                '(+ (* a c) (* a d) (* b c) (* b d))))

;; Powers of Sums: (^ (+ a b) 2) -> (+ (^ a 2) (* 2 a b) (^ b 2))
(define-test "expand-power-of-sum-binomial-square"
  (assert-equal (expand '(^ (+ a b) 2)) '(+ (* 2 a b) (^ a 2) (^ b 2)))) ; Order by simplify
(define-test "expand-power-of-sum-binomial-square-with-constants"
  ;; (^ (+ x 1) 2) -> (+ (^ x 2) (* 2 x 1) (^ 1 2)) -> (+ 1 (* 2 x) (^ x 2))
  (assert-equal (expand '(^ (+ x 1) 2)) '(+ 1 (* 2 x) (^ x 2))))
(define-test "expand-power-of-sum-not-square"
  (assert-equal (expand '(^ (+ a b) 3)) '(+ (* 3 a (^ b 2)) (* 3 b (^ a 2)) (^ a 3) (^ b 3))))
(define-test "expand-power-of-sum-not-two-terms"
  (assert-equal (expand '(^ (+ a b c) 2)) '(+ (* 2 a b) (* 2 a c) (* 2 b c) (^ a 2) (^ b 2) (^ c 2))))

;; Powers of Products: (^ (* a b ...) n) -> (* (^ a n) (^ b n) ...)
(define-test "expand-power-of-product-simple"
  (assert-equal (expand '(^ (* a b) n)) '(* (^ a n) (^ b n))))
(define-test "expand-power-of-product-nary"
  (assert-equal (expand '(^ (* a b c) 2)) '(* (^ a 2) (^ b 2) (^ c 2))))
(define-test "expand-power-of-product-with-constant-base"
  ;; (^ (* 2 x) 3) -> (* (^ 2 3) (^ x 3)) -> (* 8 (^ x 3))
  (assert-equal (expand '(^ (* 2 x) 3)) '(* 8 (^ x 3))))

;; Powers of Quotients: (^ (/ a b) n) -> (/ (^ a n) (^ b n))
(define-test "expand-power-of-quotient-simple"
  (assert-equal (expand '(^ (/ a b) n)) '(/ (^ a n) (^ b n))))
(define-test "expand-power-of-quotient-with-constants"
  ;; (^ (/ x 2) 3) -> (/ (^ x 3) (^ 2 3)) -> (/ (^ x 3) 8)
  (assert-equal (expand '(^ (/ x 2) 3)) '(/ (^ x 3) 8)))

;; Recursive Expansion and Interaction with Simplify
(define-test "expand-recursive-distribute-power"
  ;; (* a (^ (+ x y) 2)) -> (* a (+ (^ x 2) (* 2 x y) (^ y 2)))
  ;; -> (+ (* a (^ x 2)) (* a (* 2 x y)) (* a (^ y 2)))
  ;; -> (simplify result)
  (assert-equal (expand '(* a (^ (+ x y) 2)))
                '(+ (* 2 a x y) (* a (^ x 2)) (* a (^ y 2)))))
(define-test "expand-recursive-power-distribute"
  ;; Input: (^ (* a (+ b c)) 2)
  ;; Step 1 (expand base): (* a (+ b c)) -> (+ (* a b) (* a c))
  ;; Expression becomes: (^ (+ (* a b) (* a c)) 2)
  ;; Step 2 (expand power of sum): (^ (X Y) 2) -> (+ (^ X 2) (* 2 X Y) (^ Y 2))
  ;;   X = (* a b), Y = (* a c)
  ;;   -> (+ (^ (* a b) 2) (* 2 (* a b) (* a c)) (^ (* a c) 2))
  ;; Step 3 (expand terms of this sum, simplify applies to middle term):
  ;;   (^ (* a b) 2) -> (* (^ a 2) (^ b 2))
  ;;   (* 2 (* a b) (* a c)) -> simplify -> (* 2 (^ a 2) b c) ; Power grouping applied
  ;;   (^ (* a c) 2) -> (* (^ a 2) (^ c 2))
  ;; Expression becomes: (+ (* (^ a 2) (^ b 2)) (* 2 (^ a 2) b c) (* (^ a 2) (^ c 2)))
  ;; Step 4 (final simplify for ordering of sum terms):
  ;;   The terms are sorted by term<?.
  ;;   Got:      (+ (* (^ a 2) (^ b 2)) (* (^ a 2) (^ c 2)) (* 2 b c (^ a 2)))
  ;;   This is equivalent to (+ (* (^ a 2) (^ b 2)) (* (^ a 2) (^ c 2)) (* 2 (^ a 2) b c))
  ;;   after simplify-product sorts factors of the third term.
  (assert-equal (expand '(^ (* a (+ b c)) 2))
                '(+ (* (^ a 2) (^ b 2)) (* (^ a 2) (^ c 2)) (* 2 b c (^ a 2))))) ; Adapted expectation
(define-test "expand-already-expanded"
  (assert-equal (expand '(+ (* a b) (* a c))) '(+ (* a b) (* a c))))
(define-test "expand-atomic"
  (assert-equal (expand 'x) 'x))
(define-test "expand-constant"
  (assert-equal (expand 123) 123))
(define-test "expand-simple-sum"
  (assert-equal (expand '(+ a b)) '(+ a b)))

;; Test case from README example for expand
(define-test "expand-readme-example-nested-distribution"
  ;; (expand '(* 2 (+ x (* 3 (+ y 1)))))
  ;; inner expand: (* 3 (+ y 1)) -> (+ (* 3 y) (* 3 1)) -> (+ 3 (* 3 y))
  ;; expr becomes: (* 2 (+ x (+ 3 (* 3 y))))
  ;; simplify inner sum: (+ x 3 (* 3 y)) -> (+ 3 x (* 3 y))
  ;; expr becomes: (* 2 (+ 3 x (* 3 y)))
  ;; distribute 2: (+ (* 2 3) (* 2 x) (* 2 (* 3 y)))
  ;; simplify: (+ 6 (* 2 x) (* 6 y))
  (assert-equal (expand '(* 2 (+ x (* 3 (+ y 1)))))
                '(+ 6 (* 2 x) (* 6 y))))


;; Polynomial Expansion

(define-test "expand-power-of-sum-binomial-cube"
  ;; (^ (+ a b) 3) -> (+ (^ a 3) (* 3 (^ a 2) b) (* 3 a (^ b 2)) (^ b 3))
  ;; Order will be determined by simplify-sum
  (assert-equal (expand '(^ (+ a b) 3))
                '(+ (* 3 a (^ b 2)) (* 3 b (^ a 2)) (^ a 3) (^ b 3)))) ; Example order

(define-test "expand-power-of-sum-binomial-cube-with-constants"
  ;; (^ (+ x 1) 3) -> (+ (^ x 3) (* 3 (^ x 2) 1) (* 3 x (^ 1 2)) (^ 1 3))
  ;; -> (+ 1 (* 3 x) (* 3 (^ x 2)) (^ x 3))
  (assert-equal (expand '(^ (+ x 1) 3))
                '(+ 1 (* 3 (^ x 2)) (* 3 x) (^ x 3))))

(define-test "expand-power-of-sum-trinomial-square"
  ;; (^ (+ a b c) 2) -> (+ (^a 2) (^b 2) (^c 2) (* 2 a b) (* 2 a c) (* 2 b c))
  (assert-equal (expand '(^ (+ a b c) 2))
                '(+ (* 2 a b) (* 2 a c) (* 2 b c) (^ a 2) (^ b 2) (^ c 2))))

(define-test "expand-power-of-sum-binomial-fourth-power"
  ;; (^ (+ a b) 4) -> (+ (^a 4) (* 4 (^a 3)b) (* 6 (^a 2)(^b 2)) (* 4 a (^b 3)) (^b 4))
  (assert-equal (expand '(^ (+ a b) 4))
                '(+ (* 4 a (^ b 3)) (* 4 b (^ a 3)) (* 6 (^ a 2) (^ b 2)) (^ a 4) (^ b 4))))

(define-test "expand-power-of-sum-with-negation-term-squared"
  ;; (^ (+ a (- b)) 2) -> (+ (^ a 2) (* 2 a (- b)) (^ (- b) 2))
  ;; simplify -> (+ (^ a 2) (* -2 a b) (^ b 2))
  (assert-equal (expand '(^ (+ a (- b)) 2))
                '(+ (* -2 a b) (^ a 2) (^ b 2))))

(define-test "expand-power-of-product-containing-sum-squared"
  ;; (^ (* 2 (+ x y)) 2) -> (* (^ 2 2) (^ (+ x y) 2)) -> (* 4 (^ (+ x y) 2))
  ;; -> (* 4 (+ (^ x 2) (* 2 x y) (^ y 2)))
  ;; -> (+ (* 4 (^ x 2)) (* 8 x y) (* 4 (^ y 2)))
  (assert-equal (expand '(^ (* 2 (+ x y)) 2))
                '(+ (* 4 (^ x 2)) (* 4 (^ y 2)) (* 8 x y))))

;;; --- Tests for Imaginary Unit i ---

;; Powers of i
(define-test "simplify-power-i-squared"
  (assert-equal (simplify '(^ i 2)) -1))
(define-test "simplify-power-i-cubed"
  (assert-equal (simplify '(^ i 3)) '(- i)))
(define-test "simplify-power-i-fourth"
  (assert-equal (simplify '(^ i 4)) 1))
(define-test "simplify-power-i-fifth"
  (assert-equal (simplify '(^ i 5)) 'i))
(define-test "simplify-power-i-zero"
  (assert-equal (simplify '(^ i 0)) 1))
(define-test "simplify-power-i-one"
  (assert-equal (simplify '(^ i 1)) 'i))
(define-test "simplify-power-i-negative-exponent-1" ; i^-1 = i^3 = -i
  (assert-equal (simplify '(^ i -1)) '(- i)))
(define-test "simplify-power-i-negative-exponent-2" ; i^-2 = i^2 = -1
  (assert-equal (simplify '(^ i -2)) -1))
(define-test "simplify-power-i-non-integer-exponent"
  (assert-equal (simplify '(^ i x)) '(^ i x)))
(define-test "simplify-power-i-rational-exponent" ; e.g. sqrt(i) - not simplified by current rules
  (assert-equal (simplify '(^ i 1/2)) '(^ i 1/2)))

;; Products involving i
(define-test "simplify-product-i-times-i"
  (assert-equal (simplify '(* i i)) -1))
(define-test "simplify-product-i-times-i-times-i"
  (assert-equal (simplify '(* i i i)) '(- i)))
(define-test "simplify-product-i-times-i-times-i-times-i"
  (assert-equal (simplify '(* i i i i)) 1))
(define-test "simplify-product-constant-times-i-squared"
  (assert-equal (simplify '(* 3 i i)) -3))
(define-test "simplify-product-mixed-factors-with-i-squared"
  (assert-equal (simplify '(* x i y i)) '(- (* x y)))) ; simplify-product sorts to (* -1 x y)
(define-test "simplify-product-mixed-factors-with-i-cubed"
  (assert-equal (simplify '(* x i y i z i)) '(* x y z (- i)))) ; simplify-product sorts

;; Expand with i
(define-test "expand-product-i-times-i"
  (assert-equal (expand '(* i i)) -1))
(define-test "expand-power-of-sum-with-i"
  ;; (^ (+ 1 i) 2) -> (+ (^ 1 2) (* 2 1 i) (^ i 2)) -> (+ 1 (* 2 i) -1) -> (* 2 i)
  (assert-equal (expand '(^ (+ 1 i) 2)) '(* 2 i)))
(define-test "expand-product-involving-i-and-sum"
  ;; (* i (+ x i)) -> (+ (* i x) (* i i)) -> (+ (* i x) -1) -> (+ -1 (* i x))
  (assert-equal (expand '(* i (+ x i))) '(+ -1 (* i x))))

;;; --- Tests for Fractional Exponents and Sqrt ---

;; sqrt conversion to power
(define-test "simplify-sqrt-conversion"
  (assert-equal (simplify '(sqrt x)) '(^ x 1/2)))
(define-test "simplify-sqrt-of-constant" ; sqrt(4) -> (^ 4 1/2) -> simplify-power -> 2
  (assert-equal (simplify '(sqrt 4)) 2))
(define-test "simplify-sqrt-of-product" ; sqrt(* x y) -> (^ (* x y) 1/2) -> (* (^ x 1/2) (^ y 1/2))
  (assert-equal (simplify '(sqrt (* x y))) '(* (^ x 1/2) (^ y 1/2))))
(define-test "simplify-sqrt-of-power" ; sqrt(^ x 4) -> (^ (^ x 4) 1/2) -> (^ x 2)
  (assert-equal (simplify '(sqrt (^ x 4))) '(^ x 2)))

;; Simplifying roots of positive integers
(define-test "simplify-power-integer-perfect-square-root"
  (assert-equal (simplify '(^ 9 1/2)) 3))
(define-test "simplify-power-integer-perfect-cube-root"
  (assert-equal (simplify '(^ 27 1/3)) 3))
(define-test "simplify-power-integer-perfect-fourth-root"
  (assert-equal (simplify '(^ 16 1/4)) 2))
(define-test "simplify-power-integer-root-simplify-factors-sqrt12"
  ;; (^ 12 1/2) -> (^ (* 2 2 3) 1/2) -> (* (^ (2^2) 1/2) (^ 3 1/2)) -> (* 2 (^ 3 1/2))
  (assert-equal (simplify '(^ 12 1/2)) '(* 2 (^ 3 1/2))))
(define-test "simplify-power-integer-root-simplify-factors-sqrt18"
  ;; (^ 18 1/2) -> (^ (* 2 3 3) 1/2) -> (* (^ 2 1/2) (^ (3^2) 1/2)) -> (* 3 (^ 2 1/2))
  (assert-equal (simplify '(^ 18 1/2)) '(* 3 (^ 2 1/2))))
(define-test "simplify-power-integer-root-simplify-factors-sqrt72"
  ;; (^ 72 1/2) -> (^ (* 2 2 2 3 3) 1/2) -> (^ (* (2^3) (3^2)) 1/2)
  ;; -> (* (^ (2^3) 1/2) (^ (3^2) 1/2)) -> (* (^ (2^3) 1/2) 3)
  ;; -> (* 3 (^ (* 2 (2^2)) 1/2)) -> (* 3 (* (^ 2 1/2) (^ (2^2) 1/2))) -> (* 3 (* 2 (^ 2 1/2)))
  ;; -> (* 6 (^ 2 1/2))
  (assert-equal (simplify '(^ 72 1/2)) '(* 6 (^ 2 1/2))))
(define-test "simplify-power-integer-root-no-simplification"
  (assert-equal (simplify '(^ 7 1/2)) '(^ 7 1/2)))
(define-test "simplify-power-integer-root-prime-base"
  (assert-equal (simplify '(^ 5 1/3)) '(^ 5 1/3)))

;; Simplifying rational powers of positive integers
(define-test "simplify-power-integer-rational-exp-8_2_3"
  ;; (^ 8 2/3) -> (^ (2^3) 2/3) -> 2^(3*2/3) -> 2^2 -> 4
  (assert-equal (simplify '(^ 8 2/3)) 4))
(define-test "simplify-power-integer-rational-exp-32_2_5"
  ;; (^ 32 2/5) -> (^ (2^5) 2/5) -> 2^(5*2/5) -> 2^2 -> 4
  (assert-equal (simplify '(^ 32 2/5)) 4))
(define-test "simplify-power-integer-rational-exp-27_2_3"
  ;; (^ 27 2/3) -> (^ (3^3) 2/3) -> 3^(3*2/3) -> 3^2 -> 9
  (assert-equal (simplify '(^ 27 2/3)) 9))
(define-test "simplify-power-integer-rational-exp-4_3_2"
  ;; (^ 4 3/2) -> (^ (2^2) 3/2) -> 2^(2*3/2) -> 2^3 -> 8
  (assert-equal (simplify '(^ 4 3/2)) 8))
(define-test "simplify-power-integer-rational-exp-partially-simplifies"
  ;; (^ 12 3/2) -> (^ (* (2^2) 3) 3/2) -> (* (^ (2^2) 3/2) (^ 3 3/2))
  ;; -> (* (2^3) (^ 3 3/2)) -> (* 8 (^ 3 3/2))
  (assert-equal (simplify '(^ 12 3/2)) '(* 8 (^ 3 3/2))))
(define-test "simplify-power-integer-rational-exp-already-simplified-form"
  (assert-equal (simplify '(^ 5 2/3)) '(^ 5 2/3)))

;; Interaction with other power rules
(define-test "simplify-power-power-of-power-with-fractional"
  ;; (^ (^ x 6) 1/2) -> (^ x (* 6 1/2)) -> (^ x 3)
  (assert-equal (simplify '(^ (^ x 6) 1/2)) '(^ x 3)))
(define-test "simplify-power-power-of-power-with-fractional-inner"
  ;; (^ (^ x 1/2) 4) -> (^ x (* 1/2 4)) -> (^ x 2)
  (assert-equal (simplify '(^ (^ x 1/2) 4)) '(^ x 2)))
(define-test "simplify-power-power-of-product-with-fractional"
  ;; (^ (* x y) 1/2) -> (* (^ x 1/2) (^ y 1/2))
  (assert-equal (simplify '(^ (* x y) 1/2)) '(* (^ x 1/2) (^ y 1/2))))

;; Edge cases for numeric root simplification
(define-test "simplify-power-base-one-fractional-exponent"
  (assert-equal (simplify '(^ 1 1/2)) 1))
(define-test "simplify-power-base-one-complex-fractional-exponent"
  (assert-equal (simplify '(^ 1 7/13)) 1))

;;; --- Tests for Basic Predicates ---
(define-test "predicate-constant?-1"
  (assert-true (constant? 5)))
(define-test "predicate-constant?-2"
  (assert-true (constant? -10)))
(define-test "predicate-constant?-3"
  (assert-true (constant? 0)))
(define-test "predicate-constant?-4"
  (assert-true (constant? 1/2)))
(define-test "predicate-constant?-5"
  (assert-true (constant? -3/4)))
(define-test "predicate-constant?-6"
  (assert-false (constant? 'x)))
(define-test "predicate-constant?-7"
  (assert-false (constant? '(+ x 1))))

(define-test "predicate-rational?-1"
  (assert-true (rational? 5)))
(define-test "predicate-rational?-2"
  (assert-true (rational? -10)))
(define-test "predicate-rational?-3"
  (assert-true (rational? 0)))
(define-test "predicate-rational?-4"
  (assert-true (rational? 1/2)))
(define-test "predicate-rational?-5"
  (assert-true (rational? -3/4)))
(define-test "predicate-rational?-6"
  (assert-false (rational? 'x)))
(define-test "predicate-rational?-7"
  (assert-false (rational? '(+ x 1))))
(define-test "predicate-rational?-8"
  (assert-false (rational? "hello"))) ; Test with a string

(define-test "predicate-variable?-1"
  (assert-true (variable? 'x)))
(define-test "predicate-variable?-2"
  (assert-true (variable? 'my-var)))
(define-test "predicate-variable?-3"
  (assert-false (variable? '+))) ; Known operator
(define-test "predicate-variable?-4"
  (assert-false (variable? 'sqrt))) ; Known operator
(define-test "predicate-variable?-5"
  (assert-false (variable? 5)))
(define-test "predicate-variable?-6"
  (assert-false (variable? '(+ x 1))))
