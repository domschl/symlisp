;;;-----------------------------------------------------------------------------
;;; Tests for Symbolic Algebra System Core Predicates and Accessors
;;;-----------------------------------------------------------------------------

;; Load the symbolics library (assuming it's in a loadable path or same directory for testing)
;; (load "symbolics.scm") ; This might be needed depending on your test runner setup

;;; --- Tests for Predicates ---



(define-test "symbolics-constant?-is-number-positive-integer" (lambda () (assert-equal (constant? 5) #t)))
(define-test "symbolics-constant?-is-number-zero" (lambda () (assert-equal (constant? 0) #t)))
(define-test "symbolics-constant?-is-number-negative-integer" (lambda () (assert-equal (constant? -10) #t)))
(define-test "symbolics-constant?-is-number-float" (lambda () (assert-equal (constant? 3.14) #t)))
(define-test "symbolics-constant?-is-number-rational" (lambda () (assert-equal (constant? 1/2) #t)))

(define-test "symbolics-constant?-is-not-symbol" (lambda () (assert-equal (constant? 'x) #f)))
(define-test "symbolics-constant?-is-not-list" (lambda () (assert-equal (constant? '(+ 1 2)) #f)))
(define-test "symbolics-constant?-is-not-string" (lambda () (assert-equal (constant? "hello") #f)))
(define-test "symbolics-constant?-is-not-boolean" (lambda () (assert-equal (constant? #t) #f)))

(define-test "symbolics-variable?-is-symbol-simple" (lambda () (assert-equal (variable? 'x) #t)))
(define-test "symbolics-variable?-is-symbol-greek" (lambda () (assert-equal (variable? 'alpha) #t)))
(define-test "symbolics-variable?-is-symbol-with-hyphen" (lambda () (assert-equal (variable? 'my-var) #t)))

(define-test "symbolics-variable?-is-not-number" (lambda () (assert-equal (variable? 5) #f)))
(define-test "symbolics-variable?-is-not-list" (lambda () (assert-equal (variable? '(+ x 1)) #f)))
(define-test "symbolics-variable?-is-not-operator-plus" (lambda () (assert-equal (variable? '+) #f)))
(define-test "symbolics-variable?-is-not-operator-sin" (lambda () (assert-equal (variable? 'sin) #f)))

(define-test "symbolics-atomic-expr?-is-number" (lambda () (assert-equal (atomic-expr? 5) #t)))
(define-test "symbolics-atomic-expr?-is-variable" (lambda () (assert-equal (atomic-expr? 'x) #t)))

(define-test "symbolics-atomic-expr?-is-not-list" (lambda () (assert-equal (atomic-expr? '(+ 1 2)) #f)))
(define-test "symbolics-atomic-expr?-is-not-operator" (lambda () (assert-equal (atomic-expr? '+) #f)))


(define-test "symbolics-compound-expr?-is-sum" (lambda ()
  (assert-equal (compound-expr? '(+ 1 x)) #t)))
(define-test "symbolics-compound-expr?-is-product" (lambda ()
  (assert-equal (compound-expr? '(* a b c)) #t)))
(define-test "symbolics-compound-expr?-is-negation" (lambda ()
  (assert-equal (compound-expr? '(- y)) #t)))
(define-test "symbolics-compound-expr?-is-quotient" (lambda ()
  (assert-equal (compound-expr? '(/ 10 2)) #t)))
(define-test "symbolics-compound-expr?-is-power" (lambda ()
  (assert-equal (compound-expr? '(^ x 2)) #t)))
(define-test "symbolics-compound-expr?-is-sin" (lambda ()
  (assert-equal (compound-expr? '(sin x)) #t)))
(define-test "symbolics-compound-expr?-is-operator-no-args" (lambda ()
  (assert-equal (compound-expr? '(+)) #t)))


(define-test "symbolics-compound-expr?-is-not-number"
  (lambda () (assert-equal (compound-expr? 5) #f)))
(define-test "symbolics-compound-expr?-is-not-variable"
  (lambda () (assert-equal (compound-expr? 'x) #f)))
(define-test "symbolics-compound-expr?-is-not-unknown-operator"
  (lambda () (assert-equal (compound-expr? '(foo x y)) #f)))
(define-test "symbolics-compound-expr?-is-not-empty-list"
  (lambda () (assert-equal (compound-expr? '()) #f)))
(define-test "symbolics-compound-expr?-is-not-list-starting-with-number"
  (lambda () (assert-equal (compound-expr? '(1 + 2)) #f)))

(define-test "symbolics-sum?-is-sum-binary"
  (lambda () (assert-equal (sum? '(+ 1 x)) #t)))
(define-test "symbolics-sum?-is-sum-nary"
  (lambda () (assert-equal (sum? '(+ a b c d)) #t)))
(define-test "symbolics-sum?-is-sum-no-args"
  (lambda () (assert-equal (sum? '(+)) #t)))

(define-test "symbolics-sum?-is-not-product"
  (lambda () (assert-equal (sum? '(* 1 x)) #f)))
(define-test "symbolics-sum?-is-not-unknown-op"
  (lambda () (assert-equal (sum? '(plus 1 x)) #f)))
(define-test "symbolics-sum?-is-not-number"
  (lambda () (assert-equal (sum? 5) #f)))

(define-test "symbolics-product?-is-product-binary"
  (lambda () (assert-equal (product? '(* 1 x)) #t)))
(define-test "symbolics-product?-is-product-nary"
  (lambda () (assert-equal (product? '(* a b c d)) #t)))
(define-test "symbolics-product?-is-product-no-args"
  (lambda () (assert-equal (product? '(*)) #t)))

(define-test "symbolics-product?-is-not-sum"
  (lambda () (assert-equal (product? '(+ 1 x)) #f)))
(define-test "symbolics-product?-is-not-unknown-op"
  (lambda () (assert-equal (product? '(times 1 x)) #f)))
(define-test "symbolics-product?-is-not-variable"
  (lambda () (assert-equal (product? 'x) #f)))

(define-test "symbolics-power?-is-power-var-num"
  (lambda () (assert-equal (power? '(^ x 2)) #t)))
(define-test "symbolics-power?-is-power-expr-var"
  (lambda () (assert-equal (power? '(^ (+ a b) y)) #t)))

(define-test "symbolics-power?-is-not-sum"
  (lambda () (assert-equal (power? '(+ x 2)) #f)))
(define-test "symbolics-power?-is-not-unary"
  (lambda () (assert-equal (power? '(^ x)) #f)))
(define-test "symbolics-power?-is-not-ternary"
  (lambda () (assert-equal (power? '(^ x 2 3)) #f)))
(define-test "symbolics-power?-is-not-unknown-op"
  (lambda () (assert-equal (power? '(pow x 2)) #f)))

(define-test "symbolics-negation?-is-negation-var"
  (lambda () (assert-equal (negation? '(- x)) #t)))
(define-test "symbolics-negation?-is-negation-expr"
  (lambda () (assert-equal (negation? '(- (+ a b))) #t)))

(define-test "symbolics-negation?-is-not-difference"
  (lambda () (assert-equal (negation? '(- x y)) #f)))
(define-test "symbolics-negation?-is-not-no-args"
  (lambda () (assert-equal (negation? '(-)) #f)))
(define-test "symbolics-negation?-is-not-sum"
  (lambda () (assert-equal (negation? '(+ x)) #f)))

(define-test "symbolics-difference?-is-difference-var-var"
  (lambda () (assert-equal (difference? '(- x y)) #t)))
(define-test "symbolics-difference?-is-difference-num-num"
  (lambda () (assert-equal (difference? '(- 10 2)) #t)))

(define-test "symbolics-difference?-is-not-negation"
  (lambda () (assert-equal (difference? '(- x)) #f)))
(define-test "symbolics-difference?-is-not-no-args"
  (lambda () (assert-equal (difference? '(-)) #f)))
(define-test "symbolics-difference?-is-not-ternary"
  (lambda () (assert-equal (difference? '(- x y z)) #f)))
(define-test "symbolics-difference?-is-not-sum"
  (lambda () (assert-equal (difference? '(+ x y)) #f)))

(define-test "symbolics-quotient?-is-quotient-var-var"
  (lambda () (assert-equal (quotient? '(/ x y)) #t)))
(define-test "symbolics-quotient?-is-quotient-num-num"
  (lambda () (assert-equal (quotient? '(/ 10 2)) #t)))

(define-test "symbolics-quotient?-is-not-unary"
  (lambda () (assert-equal (quotient? '(/ x)) #f)))
(define-test "symbolics-quotient?-is-not-no-args"
  (lambda () (assert-equal (quotient? '(/)) #f)))
(define-test "symbolics-quotient?-is-not-ternary"
  (lambda () (assert-equal (quotient? '(/ x y z)) #f)))
(define-test "symbolics-quotient?-is-not-product"
  (lambda () (assert-equal (quotient? '(* x y)) #f)))

;;; --- Tests for Accessors ---

(define-test "symbolics-operator-accessor-sum"
  (lambda () (assert-equal (operator '(+ x 1)) '+)))
(define-test "symbolics-operator-accessor-product"
  (lambda () (assert-equal (operator '(* a b)) '*)))
;; (define-test "symbolics-operator-accessor-error-var"
;;   (lambda () (assert-error (operator 'x)))) ; assert-error not supported yet
;; (define-test "symbolics-operator-accessor-error-num"
;;   (lambda () (assert-error (operator 5))))

(define-test "symbolics-operands-accessor-sum"
  (lambda () (assert-equal (operands '(+ x 1)) '(x 1))))
(define-test "symbolics-operands-accessor-product"
  (lambda () (assert-equal (operands '(* a b c)) '(a b c))))
(define-test "symbolics-operands-accessor-sin"
  (lambda () (assert-equal (operands '(sin x)) '(x))))
(define-test "symbolics-operands-accessor-sum-no-args"
  (lambda () (assert-equal (operands '(+)) '())))
;; (define-test "symbolics-operands-accessor-error-var"
;;   (lambda () (assert-error (operands 'x))))
;; (define-test "symbolics-operands-accessor-error-num"
;;   (lambda () (assert-error (operands 5))))

(define-test "symbolics-terms-accessor-sum"
  (lambda () (assert-equal (terms '(+ x 1 y)) '(x 1 y))))
(define-test "symbolics-terms-accessor-sum-no-args"
  (lambda () (assert-equal (terms '(+)) '())))
;; (define-test "symbolics-terms-accessor-error-product"
;;   (lambda () (assert-error (terms '(* x y)))))

(define-test "symbolics-factors-accessor-product"
  (lambda () (assert-equal (factors '(* x 2 z)) '(x 2 z))))
(define-test "symbolics-factors-accessor-product-no-args"
  (lambda () (assert-equal (factors '(*)) '())))
;; (define-test "symbolics-factors-accessor-error-sum"
;;   (lambda () (assert-error (factors '(+ x y)))))

(define-test "symbolics-base-accessor-simple-power"
  (lambda () (assert-equal (base '(^ x 2)) 'x)))
(define-test "symbolics-exponent-accessor-simple-power"
  (lambda () (assert-equal (exponent '(^ x 2)) 2)))
(define-test "symbolics-base-accessor-complex-power"
  (lambda () (assert-equal (base '(^ (+ a 1) (* b 2))) '(+ a 1))))
(define-test "symbolics-exponent-accessor-complex-power"
  (lambda () (assert-equal (exponent '(^ (+ a 1) (* b 2))) '(* b 2))))
;; (define-test "symbolics-base-accessor-error-sum"
;;   (lambda () (assert-error (base '(+ x 2)))))
;; (define-test "symbolics-exponent-accessor-error-unary-power"
;;   (lambda () (assert-error (exponent '(^ x)))))

(define-test "symbolics-negated-expr-accessor-var"
  (lambda () (assert-equal (negated-expr '(- x)) 'x)))
(define-test "symbolics-negated-expr-accessor-expr"
  (lambda () (assert-equal (negated-expr '(- (* a b))) '(* a b))))
;; (define-test "symbolics-negated-expr-accessor-error-difference"
;;   (lambda () (assert-error (negated-expr '(- x y)))))

(define-test "symbolics-minuend-accessor-simple"
  (lambda () (assert-equal (minuend '(- x y)) 'x)))
(define-test "symbolics-subtrahend-accessor-simple"
  (lambda () (assert-equal (subtrahend '(- x y)) 'y)))
(define-test "symbolics-minuend-accessor-complex"
  (lambda () (assert-equal (minuend '(- 10 (+ a b))) 10)))
(define-test "symbolics-subtrahend-accessor-complex"
  (lambda () (assert-equal (subtrahend '(- 10 (+ a b))) '(+ a b))))
;; (define-test "symbolics-minuend-accessor-error-negation"
;;   (lambda () (assert-error (minuend '(- x)))))
;; (define-test "symbolics-subtrahend-accessor-error-negation"
;;   (lambda () (assert-error (subtrahend '(- x)))))

(define-test "symbolics-numerator-accessor-simple"
  (lambda () (assert-equal (quotient-numerator '(/ x y)) 'x)))
(define-test "symbolics-denominator-accessor-simple"
  (lambda () (assert-equal (quotient-denominator '(/ x y)) 'y)))
(define-test "symbolics-numerator-accessor-complex"
  (lambda () (assert-equal (quotient-numerator '(/ (* a 2) (+ c d))) '(* a 2))))
(define-test "symbolics-denominator-accessor-complex"
  (lambda () (assert-equal (quotient-denominator '(/ (* a 2) (+ c d))) '(+ c d))))
;; (define-test "symbolics-numerator-accessor-error-unary"
;;   (lambda () (assert-error (numerator '(/ x)))))
;; (define-test "symbolics-denominator-accessor-error-unary"
;;   (lambda () (assert-error (denominator '(/ x)))))

;;; --- Tests for Simplify Function ---

;; Atomic Expressions
(define-test "simplify-atomic-constant"
  (lambda () (assert-equal (simplify 5) 5)))
(define-test "simplify-atomic-variable"
  (lambda () (assert-equal (simplify 'x) 'x)))

;; Constant Folding
(define-test "simplify-constant-folding-sum"
  (lambda () (assert-equal (simplify '(+ 1 2 3)) 6)))
(define-test "simplify-constant-folding-product"
  (lambda () (assert-equal (simplify '(* 2 3 4)) 24)))
(define-test "simplify-constant-folding-difference"
  (lambda () (assert-equal (simplify '(- 10 3)) 7)))
(define-test "simplify-constant-folding-quotient"
  (lambda () (assert-equal (simplify '(/ 20 4)) 5)))
(define-test "simplify-constant-folding-power"
  (lambda () (assert-equal (simplify '(^ 2 3)) 8)))
(define-test "simplify-constant-folding-negation"
  (lambda () (assert-equal (simplify '(- 5)) -5)))

;; Arithmetic Identities - Sum
(define-test "simplify-sum-add-zero-right"
  (lambda () (assert-equal (simplify '(+ x 0)) 'x)))
(define-test "simplify-sum-add-zero-left"
  (lambda () (assert-equal (simplify '(+ 0 x)) 'x)))
(define-test "simplify-sum-add-zero-middle"
  (lambda () (assert-equal (simplify '(+ x 0 y)) '(+ x y))))
(define-test "simplify-sum-multiple-zeros"
  (lambda () (assert-equal (simplify '(+ 0 x 0 y 0)) '(+ x y))))
(define-test "simplify-sum-all-zeros"
  (lambda () (assert-equal (simplify '(+ 0 0 0)) 0)))
(define-test "simplify-sum-no-operands" ; make-sum handles this
  (lambda () (assert-equal (simplify '(+)) 0)))

;; Tests for Term Collection in Sums
(define-test "simplify-sum-collect-simple-variables"
  (lambda () (assert-equal (simplify '(+ x y x)) '(+ y (* 2 x))))) ; Assuming y < x for term<? string fallback
(define-test "simplify-sum-collect-variables-with-constants"
  (lambda () (assert-equal (simplify '(+ x 2 y 3 x 1)) '(+ 6 y (* 2 x)))))
(define-test "simplify-sum-collect-and-cancel-variables"
  (lambda () (assert-equal (simplify '(+ x y (- x))) 'y)))
(define-test "simplify-sum-collect-and-cancel-multiple-variables"
  (lambda () (assert-equal (simplify '(+ a b (- a) c (- b))) 'c)))
(define-test "simplify-sum-collect-terms-with-coefficients"
  (lambda () (assert-equal (simplify '(+ (* 2 x) y (* 3 x))) '(+ y (* 5 x)))))
(define-test "simplify-sum-collect-terms-with-negative-coefficients"
  (lambda () (assert-equal (simplify '(+ (* 5 x) y (* -2 x))) '(+ y (* 3 x)))))
(define-test "simplify-sum-collect-and-cancel-terms-with-coefficients"
  (lambda () (assert-equal (simplify '(+ (* 2 x) y (* -2 x))) 'y)))
(define-test "simplify-sum-collect-complex-terms"
  ;; (+ (^ x 2) (* 3 (^ x 2)) y) -> (+ y (* 4 (^ x 2)))
  (lambda () (assert-equal (simplify '(+ (^ x 2) (* 3 (^ x 2)) y)) '(+ y (* 4 (^ x 2))))))
(define-test "simplify-sum-collect-from-readme-example"
  ;; (simplify '(+ x (* -1 y) (* 2 x) y)) => (* 3 x)
  (lambda () (assert-equal (simplify '(+ x (* -1 y) (* 2 x) y)) '(* 3 x))))
(define-test "simplify-sum-collect-all-cancel-to-zero"
  (lambda () (assert-equal (simplify '(+ x y (- x) (- y))) 0)))
(define-test "simplify-sum-collect-with-constants-cancelling-variables"
  (lambda () (assert-equal (simplify '(+ 5 x y (- x) (- y) 2)) 7)))
(define-test "simplify-sum-collect-nested-and-flattened"
  ;; (+ x (+ y (* 2 x)) (- y) z) -> (+ x y (* 2 x) (- y) z) -> (+ z (* 3 x))
  (lambda () (assert-equal (simplify '(+ x (+ y (* 2 x)) (- y) z)) '(+ z (* 3 x)))))
(define-test "simplify-sum-collect-with-negation-terms"
  ;; (+ a (- b) (* 2 a) b) -> (+ b (- b) (* 3 a)) -> (* 3 a)
  (lambda () (assert-equal (simplify '(+ a (- b) (* 2 a) b)) '(* 3 a))))
(define-test "simplify-sum-collect-leading-to-single-negative-term"
  (lambda () (assert-equal (simplify '(+ (* 2 x) (* -3 x))) '(- x))))
(define-test "simplify-sum-collect-leading-to-single-constant"
  (lambda () (assert-equal (simplify '(+ x 5 (- x))) 5)))

;; Arithmetic Identities - Product
(define-test "simplify-product-mult-one-right"
  (lambda () (assert-equal (simplify '(* x 1)) 'x)))
(define-test "simplify-product-mult-one-left"
  (lambda () (assert-equal (simplify '(* 1 x)) 'x)))
(define-test "simplify-product-mult-one-middle"
  (lambda () (assert-equal (simplify '(* x 1 y)) '(* x y))))
(define-test "simplify-product-multiple-ones"
  (lambda () (assert-equal (simplify '(* 1 x 1 y 1)) '(* x y))))
(define-test "simplify-product-all-ones"
  (lambda () (assert-equal (simplify '(* 1 1 1)) 1)))
(define-test "simplify-product-mult-zero-right"
  (lambda () (assert-equal (simplify '(* x 0)) 0)))
(define-test "simplify-product-mult-zero-left"
  (lambda () (assert-equal (simplify '(* 0 x)) 0)))
(define-test "simplify-product-mult-zero-middle"
  (lambda () (assert-equal (simplify '(* x 0 y)) 0)))
(define-test "simplify-product-no-operands" ; make-product handles this
  (lambda () (assert-equal (simplify '(*)) 1)))

;; Arithmetic Identities - Power
(define-test "simplify-power-exponent-one"
  (lambda () (assert-equal (simplify '(^ x 1)) 'x)))
(define-test "simplify-power-exponent-zero"
  (lambda () (assert-equal (simplify '(^ x 0)) 1)))
(define-test "simplify-power-base-one"
  (lambda () (assert-equal (simplify '(^ 1 x)) 1)))
(define-test "simplify-power-base-zero-positive-exponent" ; 0^x = 0 for x > 0
  (lambda () (assert-equal (simplify '(^ 0 2)) 0)))
(define-test "simplify-power-base-zero-variable-exponent" 
  (lambda () (assert-equal (simplify '(^ 0 x)) '(^ 0 x))))
(define-test "simplify-power-zero-to-zero" ; 0^0 = 1
  (lambda () (assert-equal (simplify '(^ 0 0)) 1)))


;; Arithmetic Identities - Negation
(define-test "simplify-negation-of-zero"
  (lambda () (assert-equal (simplify '(- 0)) 0)))
(define-test "simplify-negation-double"
  (lambda () (assert-equal (simplify '(- (- x))) 'x)))
(define-test "simplify-negation-of-product-with-constant"
  (lambda () (assert-equal (simplify '(- (* 2 x))) '(* -2 x))))
(define-test "simplify-negation-of-product-with-negative-constant"
  (lambda () (assert-equal (simplify '(- (* -2 x))) '(* 2 x))))

;; Arithmetic Identities - Difference
(define-test "simplify-difference-subtract-zero"
  (lambda () (assert-equal (simplify '(- x 0)) 'x)))
(define-test "simplify-difference-zero-subtract-x" ; -> (- x)
  (lambda () (assert-equal (simplify '(- 0 x)) '(- x))))
(define-test "simplify-difference-subtract-self"
  (lambda () (assert-equal (simplify '(- x x)) 0)))
(define-test "simplify-difference-canonical-form" ; (- a b) -> (+ a (* -1 b)) -> (+ a (- b))
  (lambda () (assert-equal (simplify '(- a b)) '(+ a (- b)))))
(define-test "simplify-difference-constants-to-canonical"
  (lambda () (assert-equal (simplify '(- 2 5)) -3))) ; Should fold, but if not, canonical is (+ 2 (- 5))

;; Arithmetic Identities - Quotient
(define-test "simplify-quotient-divide-by-one"
  (lambda () (assert-equal (simplify '(/ x 1)) 'x)))
(define-test "simplify-quotient-zero-divided-by-x"
  (lambda () (assert-equal (simplify '(/ 0 x)) 0)))
(define-test "simplify-quotient-divide-self" ; x/x = 1 (assuming x != 0)
  (lambda () (assert-equal (simplify '(/ x x)) 1)))
(define-test "simplify-quotient-divide-by-zero-remains" ; Or error, current returns unsimplified
  (lambda () (assert-equal (simplify '(/ x 0)) '(/ x 0))))

;;; --- Tests for term<? canonical ordering ---
(define-test "term<?-constants-1"
  (lambda () (assert-true (term<? 1 2))))
(define-test "term<?-constants-2"
  (lambda () (assert-false (term<? 2 1))))
(define-test "term<?-constants-3"
  (lambda () (assert-false (term<? 1 1))))

(define-test "term<?-constant-vs-variable-1"
  (lambda () (assert-true (term<? 1 'x))))
(define-test "term<?-constant-vs-variable-2"
  (lambda () (assert-false (term<? 'x 1))))

(define-test "term<?-constant-vs-compound-1"
  (lambda () (assert-true (term<? 1 '(+ x 1)))))
(define-test "term<?-constant-vs-compound-2"
  (lambda () (assert-false (term<? '(+ x 1) 1))))

(define-test "term<?-variables-1"
  (lambda () (assert-true (term<? 'a 'b))))
(define-test "term<?-variables-2"
  (lambda () (assert-false (term<? 'b 'a))))
(define-test "term<?-variables-3"
  (lambda () (assert-false (term<? 'a 'a))))
(define-test "term<?-variables-4"
  (lambda () (assert-true (term<? 'x 'y))))  ; Assuming standard alphabetical
(define-test "term<?-variables-5"
  (lambda () (assert-true (term<? 'y 'z)))) ; Assuming standard alphabetical

(define-test "term<?-variable-vs-compound-1"
  (lambda () (assert-true (term<? 'x '(+ x 1)))))  ; Variable < Compound
(define-test "term<?-variable-vs-compound-2"
  (lambda () (assert-false (term<? '(+ x 1) 'x)))) ; Compound not < Variable

;; Recursive Simplification
(define-test "simplify-recursive-sum-product"
  (lambda () (assert-equal (simplify '(+ (* 2 3) x)) '(+ 6 x))))
(define-test "simplify-recursive-product-sum"
  (lambda () (assert-equal (simplify '(* (+ 1 2) y)) '(* 3 y))))
(define-test "simplify-recursive-power-sum-diff"
  (lambda () (assert-equal (simplify '(^ (+ 1 1) (- 5 2))) 8))) ; (^ 2 3) -> 8
(define-test "simplify-recursive-sum-of-zeros"
  (lambda () (assert-equal (simplify '(+ (* x 0) (* 0 y))) 0)))
(define-test "simplify-recursive-product-of-ones"
  (lambda () (assert-equal (simplify '(* (^ x 0) (^ y 0))) 1)))

;; Combination of Rules
(define-test "simplify-combo-sum-prod-identities"
  (lambda () (assert-equal (simplify '(+ (* x 1) (* y 0) z)) '(+ x z))))
(define-test "simplify-combo-diff-prod-negation" ; (- (* 5 x) (- 0 y)) -> (+ (* 5 x) (- (- y))) -> (+ (* 5 x) y)
  (lambda () (assert-equal (simplify '(- (* 5 x) (- 0 y))) '(+ y (* 5 x)))))
(define-test "simplify-combo-quotient-power"
  (lambda () (assert-equal (simplify '(/ (^ x 1) (^ y 0))) 'x))) ; (/ x 1) -> x

;; Expressions that are already simple or cannot be simplified further by current rules
(define-test "simplify-already-simple-sum"
  (lambda () (assert-equal (simplify '(+ x y)) '(+ x y))))
(define-test "simplify-already-simple-product"
  (lambda () (assert-equal (simplify '(* x y)) '(* x y))))
(define-test "simplify-already-simple-power"
  (lambda () (assert-equal (simplify '(^ x y)) '(^ x y))))
(define-test "simplify-unknown-function-simplifies-args"
  (lambda () (assert-equal (simplify '(foo (+ 1 0) (* 2 3))) '(foo 1 6)))) 

(define-test "simplify-unknown-function-simplifies-args-example2"
  (lambda () (assert-equal (simplify '(sin (+ x 0))) '(sin x))))


;; Test canonicalization of difference via sum
(define-test "simplify-difference-complex-to-canonical"
  (lambda () (assert-equal (simplify '(- (+ x 1) (+ y 2))) '(+ -1 x (- y))))) ; Adapted expectation

;; Test product with -1 constant factor
(define-test "simplify-product-with-minus-one-constant"
  (lambda () (assert-equal (simplify '(* -1 x y)) '(- (* x y)))))
(define-test "simplify-product-with-constants-evaluating-to-minus-one"
  (lambda () (assert-equal (simplify '(* 1 -1 x y)) '(- (* x y)))))

;; Test sum with constants and non-constants
(define-test "simplify-sum-constants-and-non-constants"
  (lambda () (assert-equal (simplify '(+ 2 x 3 y 4)) '(+ 9 x y))))
(define-test "simplify-sum-only-non-constants"
  (lambda () (assert-equal (simplify '(+ x y z)) '(+ x y z))))
(define-test "simplify-sum-one-non-constant-and-zero"
  (lambda () (assert-equal (simplify '(+ x 0)) 'x)))
(define-test "simplify-sum-one-non-constant-and-constants"
  (lambda () (assert-equal (simplify '(+ 2 x 3)) '(+ 5 x))))

;; Test product with constants and non-constants
(define-test "simplify-product-constants-and-non-constants"
  (lambda () (assert-equal (simplify '(* 2 x 3 y 4)) '(* 24 x y))))
(define-test "simplify-product-only-non-constants"
  (lambda () (assert-equal (simplify '(* x y z)) '(* x y z))))
(define-test "simplify-product-one-non-constant-and-one"
  (lambda () (assert-equal (simplify '(* x 1)) 'x)))
(define-test "simplify-product-one-non-constant-and-constants"
  (lambda () (assert-equal (simplify '(* 2 x 3)) '(* 6 x))))

;; Test for (- 0 x) -> (- x)
(define-test "simplify-zero-minus-variable-is-negation"
  (lambda () (assert-equal (simplify '(- 0 var)) '(- var))))

;; Test for (- (- x)) -> x in various contexts
(define-test "simplify-sum-with-double-negation"
  (lambda () (assert-equal (simplify '(+ a (- (- b)))) '(+ a b))))

;; Test for (- (* c x)) -> (* (- c) x)
(define-test "simplify-negation-of-product-positive-const"
  (lambda () (assert-equal (simplify '(- (* 5 k))) '(* -5 k))))
(define-test "simplify-negation-of-product-negative-const"
  (lambda () (assert-equal (simplify '(- (* -5 k))) '(* 5 k))))

;; Test for simplify-difference converting to (+ a (* -1 b)) and then simplifying that
(define-test "simplify-diff-a-minus-const-b"
  (lambda () (assert-equal (simplify '(- a 5)) '(+ -5 a)))) ; Adapted expectation
(define-test "simplify-diff-const-a-minus-b"
  (lambda () (assert-equal (simplify '(- 5 b)) '(+ 5 (- b)))))
(define-test "simplify-diff-a-minus-neg-b" ; (- a (- b)) -> (+ a (- (* -1 (- b)))) -> (+ a (- (- b))) -> (+ a b)
  (lambda () (assert-equal (simplify '(- a (- b))) '(+ a b))))

;;; --- Tests for Canonical Forms: Flattening and Ordering ---

;; Flattening Sums
(define-test "simplify-flatten-sum-simple"
  (lambda () (assert-equal (simplify '(+ x (+ y z))) '(+ x y z)))) ; Assuming x < y < z
(define-test "simplify-flatten-sum-nested-right"
  (lambda () (assert-equal (simplify '(+ a (+ b (+ c d)))) '(+ a b c d)))) ; Assuming a < b < c < d
(define-test "simplify-flatten-sum-nested-left"
  (lambda () (assert-equal (simplify '(+ (+ (+ a b) c) d)) '(+ a b c d))))
(define-test "simplify-flatten-sum-with-constants"
  (lambda () (assert-equal (simplify '(+ 1 (+ x 2 (+ y 3)))) '(+ 6 x y)))) ; Constants collected, then sorted with vars
(define-test "simplify-flatten-sum-identities-involved"
  (lambda () (assert-equal (simplify '(+ x (+ 0 y (+ z 0)))) '(+ x y z))))

;; Flattening Products
(define-test "simplify-flatten-product-simple"
  (lambda () (assert-equal (simplify '(* x (* y z))) '(* x y z)))) ; Assuming x < y < z
(define-test "simplify-flatten-product-nested-right"
  (lambda () (assert-equal (simplify '(* a (* b (* c d)))) '(* a b c d)))) ; Assuming a < b < c < d
(define-test "simplify-flatten-product-nested-left"
  (lambda () (assert-equal (simplify '(* (* (* a b) c) d)) '(* a b c d))))
(define-test "simplify-flatten-product-with-constants"
  (lambda () (assert-equal (simplify '(* 2 (* x 3 (* y 4)))) '(* 24 x y)))) ; Constants collected, then sorted
(define-test "simplify-flatten-product-identities-involved"
  (lambda () (assert-equal (simplify '(* x (* 1 y (* z 1)))) '(* x y z))))
(define-test "simplify-flatten-product-zero-involved"
  (lambda () (assert-equal (simplify '(* x (* 0 y (* z 1)))) 0)))

;; Ordering Terms in Sums (relies on term<?)
(define-test "simplify-order-sum-vars"
  (lambda () (assert-equal (simplify '(+ z y x)) '(+ x y z)))
  )
(define-test "simplify-order-sum-const-vars"
  (lambda () (assert-equal (simplify '(+ y 10 x 5)) '(+ 15 x y)))
  )
(define-test "simplify-order-sum-const-vars-compounds"
  (lambda () (assert-equal (simplify '(+ (foo b) z 20 y 10 x (bar a)))
                '(+ 30 x y z (bar a) (foo b)))) ; Order of (bar a) (foo b) depends on object->string
  )
;; Ordering Factors in Products (relies on term<?)
(define-test "simplify-order-product-vars"
  (lambda () (assert-equal (simplify '(* z y x)) '(* x y z)))
  )
(define-test "simplify-order-product-const-vars"
  (lambda () (assert-equal (simplify '(* y 10 x 5)) '(* 50 x y)))
  )
(define-test "simplify-order-product-const-vars-compounds"
  (lambda () (assert-equal (simplify '(* (foo b) z 20 y 10 x (bar a)))
                '(* 200 x y z (bar a) (foo b)))) ; Order of (bar a) (foo b) depends on object->string
  )

;;; --- Tests for Power-Grouping in Products (via simplify-product) ---
(define-test "simplify-product-group-single-variable-pair"
  (lambda () (assert-equal (simplify '(* x x)) '(^ x 2))))
(define-test "simplify-product-group-single-variable-triple"
  (lambda () (assert-equal (simplify '(* y y y)) '(^ y 3))))
(define-test "simplify-product-group-with-other-factors"
  (lambda () (assert-equal (simplify '(* a x x b x c)) '(* a b c (^ x 3)))) ; Assumes a,b,c sorted before x
  )
(define-test "simplify-product-group-multiple-variables"
  (lambda () (assert-equal (simplify '(* a b a b a)) '(* (^ a 3) (^ b 2))))
  )
(define-test "simplify-product-group-with-constants"
  (lambda () (assert-equal (simplify '(* 2 x 3 x)) '(* 6 (^ x 2))))
  )
(define-test "simplify-product-group-no-grouping-needed"
  (lambda () (assert-equal (simplify '(* a b c)) '(* a b c)))
  )
(define-test "simplify-product-group-as-power"
  (lambda () (assert-equal (simplify '(* (^ x 2) x)) '(^ x 3)))
  )
(define-test "simplify-product-group-complex-factors"
  ;; Input: (* (+ a 1) x (+ a 1) y (+ a 1))
  ;; simplify on (+ a 1) -> (+ 1 a)
  ;; Sorted factors before grouping: (x y (+ 1 a) (+ 1 a) (+ 1 a))
  ;; Grouped: (x y (^ (+ 1 a) 3))
  (lambda ()
  (assert-equal (simplify '(* (+ a 1) x (+ a 1) y (+ a 1)))
                '(* x y (^ (+ 1 a) 3))))) ; Adapted: (+ 1 a) due to inner simplify
(define-test "simplify-product-group-flatten-then-group"
  ;; Input: (* x (* y x y) z x) -> flattened to (* x y x y z x)
  ;; Sorted non-constant factors: (x x x y y z) (assuming alphabetical for vars)
  ;; Grouped: ((^ x 3) (^ y 2) z)
  ;; term<? sorts z (var) before (^ x 3) and (^ y 2) (compounds)
  ;; Then (^ x 3) vs (^ y 2) by object->string. Assume (^ x 3) < (^ y 2)
  (lambda ()
  (assert-equal (simplify '(* x (* y x y) z x)) '(* z (^ x 3) (^ y 2))))) ; Adapted based on term<?

;; Combined Flattening and Ordering
(define-test "simplify-flatten-order-sum-complex"
  (lambda () (assert-equal (simplify '(+ c (+ 10 a) 5 (+ b x))) '(+ 15 a b c x))))
(define-test "simplify-flatten-order-product-complex"
  (lambda () (assert-equal (simplify '(* c (* 10 a) 5 (* b x))) '(* 50 a b c x))))

;; Interaction with (- a b) -> (+ a (* -1 b))
(define-test "simplify-flatten-order-after-difference-conversion"
  (lambda () (assert-equal (simplify '(- (+ z 3) (+ x 1))) '(+ 2 z (- x)))))
(define-test "simplify-flatten-order-sum-with-negations"
  ;; (+ c (- b) a) -> (+ a (- b) c) (if a < c and (- b) is treated as compound after vars)
  ;; or (+ (- b) a c) (if (-b) comes before a and c due to term<?)
  ;; Assuming variables first, then compounds:
  (lambda () (assert-equal (simplify '(+ c (- b) a)) '(+ a c (- b)))))

;;; --- Tests for Expand Function ---

;; Distributive Property: (* a (+ b c ...)) -> (+ (* a b) (* a c) ...)
(define-test "expand-distribute-simple"
  (lambda () (assert-equal (expand '(* a (+ b c))) '(+ (* a b) (* a c)))))
(define-test "expand-distribute-constant-factor"
  (lambda () (assert-equal (expand '(* 2 (+ x y))) '(+ (* 2 x) (* 2 y)))))
(define-test "expand-distribute-nary-sum"
  (lambda () (assert-equal (expand '(* a (+ b c d))) '(+ (* a b) (* a c) (* a d)))))
(define-test "expand-distribute-sum-first" ; Should reorder due to simplify
  (lambda () (assert-equal (expand '(* (+ b c) a)) '(+ (* a b) (* a c)))))
(define-test "expand-distribute-multiple-factors"
  (lambda () (assert-equal (expand '(* a x (+ b c) y)) '(+ (* a b x y) (* a c x y)))))
(define-test "expand-distribute-no-sum"
  (lambda () (assert-equal (expand '(* a b c)) '(* a b c))))
(define-test "expand-distribute-sum-with-one-term" ; simplify might turn (+ x) to x
  (lambda () (assert-equal (expand '(* a (+ x))) '(* a x))))
(define-test "expand-distribute-product-of-sums"
  (lambda () 
    ;; (* (+ a b) (+ c d)) -> expand first sum: (+ (* (+ a b) c) (* (+ a b) d))
    ;; -> simplify: (+ (* c (+ a b)) (* d (+ a b)))
    ;; -> expand again: (+ (+ (* a c) (* b c)) (+ (* a d) (* b d)))
    ;; -> simplify: (+ (* a c) (* a d) (* b c) (* b d)) (order depends on term<?)
    (assert-equal (expand '(* (+ a b) (+ c d)))
                '(+ (* a c) (* a d) (* b c) (* b d)))))

;; Powers of Sums: (^ (+ a b) 2) -> (+ (^ a 2) (* 2 a b) (^ b 2))
(define-test "expand-power-of-sum-binomial-square"
  (lambda () (assert-equal (expand '(^ (+ a b) 2)) '(+ (* 2 a b) (^ a 2) (^ b 2))))) ; Order by simplify
(define-test "expand-power-of-sum-binomial-square-with-constants"
  (lambda () 
    ;; (^ (+ x 1) 2) -> (+ (^ x 2) (* 2 x 1) (^ 1 2)) -> (+ 1 (* 2 x) (^ x 2))
    (assert-equal (expand '(^ (+ x 1) 2)) '(+ 1 (* 2 x) (^ x 2)))))
(define-test "expand-power-of-sum-not-square"
  (lambda () (assert-equal (expand '(^ (+ a b) 3)) '(+ (* 3 a (^ b 2)) (* 3 b (^ a 2)) (^ a 3) (^ b 3)))))
(define-test "expand-power-of-sum-not-two-terms"
  (lambda () (assert-equal (expand '(^ (+ a b c) 2)) '(+ (* 2 a b) (* 2 a c) (* 2 b c) (^ a 2) (^ b 2) (^ c 2)))))

;; Powers of Products: (^ (* a b ...) n) -> (* (^ a n) (^ b n) ...)
(define-test "expand-power-of-product-simple"
  (lambda () (assert-equal (expand '(^ (* a b) n)) '(* (^ a n) (^ b n)))))
(define-test "expand-power-of-product-nary"
  (lambda () (assert-equal (expand '(^ (* a b c) 2)) '(* (^ a 2) (^ b 2) (^ c 2)))))
(define-test "expand-power-of-product-with-constant-base"
  (lambda ()
    ;; (^ (* 2 x) 3) -> (* (^ 2 3) (^ x 3)) -> (* 8 (^ x 3))
    (assert-equal (expand '(^ (* 2 x) 3)) '(* 8 (^ x 3)))))

;; Powers of Quotients: (^ (/ a b) n) -> (/ (^ a n) (^ b n))
(define-test "expand-power-of-quotient-simple"
  (lambda () (assert-equal (expand '(^ (/ a b) n)) '(/ (^ a n) (^ b n)))))
(define-test "expand-power-of-quotient-with-constants"
  (lambda ()
    ;; (^ (/ x 2) 3) -> (/ (^ x 3) (^ 2 3)) -> (/ (^ x 3) 8)
    (assert-equal (expand '(^ (/ x 2) 3)) '(/ (^ x 3) 8))))

;; Recursive Expansion and Interaction with Simplify
(define-test "expand-recursive-distribute-power"
  (lambda ()
    ;; (* a (^ (+ x y) 2)) -> (* a (+ (^ x 2) (* 2 x y) (^ y 2)))
    ;; -> (+ (* a (^ x 2)) (* a (* 2 x y)) (* a (^ y 2)))
    ;; -> (simplify result)
    (assert-equal (expand '(* a (^ (+ x y) 2)))
                  '(+ (* 2 a x y) (* a (^ x 2)) (* a (^ y 2))))))
(define-test "expand-recursive-power-distribute"
  (lambda ()
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
                  '(+ (* (^ a 2) (^ b 2)) (* (^ a 2) (^ c 2)) (* 2 b c (^ a 2)))))) ; Adapted expectation
(define-test "expand-already-expanded"
  (lambda () (assert-equal (expand '(+ (* a b) (* a c))) '(+ (* a b) (* a c)))))
(define-test "expand-atomic"
  (lambda () (assert-equal (expand 'x) 'x)))
(define-test "expand-constant"
  (lambda () (assert-equal (expand 123) 123)))
(define-test "expand-simple-sum"
  (lambda () (assert-equal (expand '(+ a b)) '(+ a b))))

;; Test case from README example for expand
(define-test "expand-readme-example-nested-distribution"
  (lambda ()
    ;; (expand '(* 2 (+ x (* 3 (+ y 1)))))
    ;; inner expand: (* 3 (+ y 1)) -> (+ (* 3 y) (* 3 1)) -> (+ 3 (* 3 y))
    ;; expr becomes: (* 2 (+ x (+ 3 (* 3 y))))
    ;; simplify inner sum: (+ x 3 (* 3 y)) -> (+ 3 x (* 3 y))
    ;; expr becomes: (* 2 (+ 3 x (* 3 y)))
    ;; distribute 2: (+ (* 2 3) (* 2 x) (* 2 (* 3 y)))
    ;; simplify: (+ 6 (* 2 x) (* 6 y))
    (assert-equal (expand '(* 2 (+ x (* 3 (+ y 1)))))
                '(+ 6 (* 2 x) (* 6 y)))))


;; Polynomial Expansion

(define-test "expand-power-of-sum-binomial-cube"
  (lambda ()
    ;; (^ (+ a b) 3) -> (+ (^ a 3) (* 3 (^ a 2) b) (* 3 a (^ b 2)) (^ b 3))
    ;; Order will be determined by simplify-sum
    (assert-equal (expand '(^ (+ a b) 3))
                '(+ (* 3 a (^ b 2)) (* 3 b (^ a 2)) (^ a 3) (^ b 3))))) ; Example order

(define-test "expand-power-of-sum-binomial-cube-with-constants"
  (lambda ()
    ;; (^ (+ x 1) 3) -> (+ (^ x 3) (* 3 (^ x 2) 1) (* 3 x (^ 1 2)) (^ 1 3))
    ;; -> (+ 1 (* 3 x) (* 3 (^ x 2)) (^ x 3))
    (assert-equal (expand '(^ (+ x 1) 3))
                '(+ 1 (* 3 (^ x 2)) (* 3 x) (^ x 3)))))

(define-test "expand-power-of-sum-trinomial-square"
  (lambda ()
    ;; (^ (+ a b c) 2) -> (+ (^a 2) (^b 2) (^c 2) (* 2 a b) (* 2 a c) (* 2 b c))
    (assert-equal (expand '(^ (+ a b c) 2))
                '(+ (* 2 a b) (* 2 a c) (* 2 b c) (^ a 2) (^ b 2) (^ c 2)))))

(define-test "expand-power-of-sum-binomial-fourth-power"
  (lambda ()
    ;; (^ (+ a b) 4) -> (+ (^a 4) (* 4 (^a 3)b) (* 6 (^a 2)(^b 2)) (* 4 a (^b 3)) (^b 4))
    (assert-equal (expand '(^ (+ a b) 4))
                '(+ (* 4 a (^ b 3)) (* 4 b (^ a 3)) (* 6 (^ a 2) (^ b 2)) (^ a 4) (^ b 4)))))

(define-test "expand-power-of-sum-with-negation-term-squared"
  (lambda ()
    ;; (^ (+ a (- b)) 2) -> (+ (^ a 2) (* 2 a (- b)) (^ (- b) 2))
    ;; simplify -> (+ (^ a 2) (* -2 a b) (^ b 2))
    (assert-equal (expand '(^ (+ a (- b)) 2))
                '(+ (* -2 a b) (^ a 2) (^ b 2)))))

(define-test "expand-power-of-product-containing-sum-squared"
  (lambda ()
    ;; (^ (* 2 (+ x y)) 2) -> (* (^ 2 2) (^ (+ x y) 2)) -> (* 4 (^ (+ x y) 2))
    ;; -> (* 4 (+ (^ x 2) (* 2 x y) (^ y 2)))
    ;; -> (+ (* 4 (^ x 2)) (* 8 x y) (* 4 (^ y 2)))
    (assert-equal (expand '(^ (* 2 (+ x y)) 2))
                '(+ (* 4 (^ x 2)) (* 4 (^ y 2)) (* 8 x y)))))

;;; --- Tests for Imaginary Unit i ---

;; Powers of i
(define-test "simplify-power-i-squared"
  (lambda () (assert-equal (simplify '(^ i 2)) -1)))
(define-test "simplify-power-i-cubed"
  (lambda () (assert-equal (simplify '(^ i 3)) '(- i))))
(define-test "simplify-power-i-fourth"
  (lambda () (assert-equal (simplify '(^ i 4)) 1)))
(define-test "simplify-power-i-fifth"
  (lambda () (assert-equal (simplify '(^ i 5)) 'i)))
(define-test "simplify-power-i-zero"
  (lambda () (assert-equal (simplify '(^ i 0)) 1)))
(define-test "simplify-power-i-one"
  (lambda () (assert-equal (simplify '(^ i 1)) 'i)))
(define-test "simplify-power-i-negative-exponent-1" ; i^-1 = i^3 = -i
  (lambda () (assert-equal (simplify '(^ i -1)) '(- i))))
(define-test "simplify-power-i-negative-exponent-2" ; i^-2 = i^2 = -1
  (lambda () (assert-equal (simplify '(^ i -2)) -1)))
(define-test "simplify-power-i-non-integer-exponent"
  (lambda () (assert-equal (simplify '(^ i x)) '(^ i x))))
(define-test "simplify-power-i-rational-exponent" ; e.g. sqrt(i) - not simplified by current rules
  (lambda () (assert-equal (simplify '(^ i 1/2)) '(^ i 1/2))))

;; Products involving i
(define-test "simplify-product-i-times-i"
  (lambda () (assert-equal (simplify '(* i i)) -1)))
(define-test "simplify-product-i-times-i-times-i"
  (lambda () (assert-equal (simplify '(* i i i)) '(- i))))
(define-test "simplify-product-i-times-i-times-i-times-i"
  (lambda () (assert-equal (simplify '(* i i i i)) 1)))
(define-test "simplify-product-constant-times-i-squared"
  (lambda () (assert-equal (simplify '(* 3 i i)) -3)))
(define-test "simplify-product-mixed-factors-with-i-squared"
  (lambda () (assert-equal (simplify '(* x i y i)) '(- (* x y))))) ; simplify-product sorts to (* -1 x y)
(define-test "simplify-product-mixed-factors-with-i-cubed"
  (lambda () (assert-equal (simplify '(* x i y i z i)) '(- (* i x y z))))) ; simplify-product sorts

;; Expand with i
(define-test "expand-product-i-times-i"
  (lambda () (assert-equal (expand '(* i i)) -1)))
(define-test "expand-power-of-sum-with-i"
  (lambda () 
    ;; (^ (+ 1 i) 2) -> (+ (^ 1 2) (* 2 1 i) (^ i 2)) -> (+ 1 (* 2 i) -1) -> (* 2 i)
    (assert-equal (expand '(^ (+ 1 i) 2)) '(* 2 i))))
(define-test "expand-product-involving-i-and-sum"
  (lambda ()
    ;; (* i (+ x i)) -> (+ (* i x) (* i i)) -> (+ (* i x) -1) -> (+ -1 (* i x))
    (assert-equal (expand '(* i (+ x i))) '(+ -1 (* i x)))))

;;; --- Tests for Fractional Exponents and Sqrt ---

;; sqrt conversion to power
(define-test "simplify-sqrt-conversion"
  (lambda () (assert-equal (simplify '(sqrt x)) '(^ x 1/2))))
(define-test "simplify-sqrt-of-constant" ; sqrt(4) -> (^ 4 1/2) -> simplify-power -> 2
  (lambda () (assert-equal (simplify '(sqrt 4)) 2)))
(define-test "simplify-sqrt-of-product" ; sqrt(* x y) -> (^ (* x y) 1/2) -> (* (^ x 1/2) (^ y 1/2))
  (lambda () (assert-equal (simplify '(sqrt (* x y))) '(* (^ x 1/2) (^ y 1/2)))))
(define-test "simplify-sqrt-of-power" ; sqrt(^ x 4) -> (^ (^ x 4) 1/2) -> (^ x 2)
  (lambda () (assert-equal (simplify '(sqrt (^ x 4))) '(^ x 2))))

;; Simplifying roots of positive integers
(define-test "simplify-power-integer-perfect-square-root"
  (lambda () (assert-equal (simplify '(^ 9 1/2)) 3)))
(define-test "simplify-power-integer-perfect-cube-root"
  (lambda () (assert-equal (simplify '(^ 27 1/3)) 3)))
(define-test "simplify-power-integer-perfect-fourth-root"
  (lambda () (assert-equal (simplify '(^ 16 1/4)) 2)))
(define-test "simplify-power-integer-root-simplify-factors-sqrt12"
  (lambda () 
    ;; (^ 12 1/2) -> (^ (* 2 2 3) 1/2) -> (* (^ (2^2) 1/2) (^ 3 1/2)) -> (* 2 (^ 3 1/2))
    (assert-equal (simplify '(^ 12 1/2)) '(* 2 (^ 3 1/2)))))
(define-test "simplify-power-integer-root-simplify-factors-sqrt18"
  (lambda ()
    ;; (^ 18 1/2) -> (^ (* 2 3 3) 1/2) -> (* (^ 2 1/2) (^ (3^2) 1/2)) -> (* 3 (^ 2 1/2))
    (assert-equal (simplify '(^ 18 1/2)) '(* 3 (^ 2 1/2)))))
(define-test "simplify-power-integer-root-simplify-factors-sqrt72"
  (lambda ()
    ;; (^ 72 1/2) -> (^ (* 2 2 2 3 3) 1/2) -> (^ (* (2^3) (3^2)) 1/2)
    ;; -> (* (^ (2^3) 1/2) (^ (3^2) 1/2)) -> (* (^ (2^3) 1/2) 3)
    ;; -> (* 3 (^ (* 2 (2^2)) 1/2)) -> (* 3 (* (^ 2 1/2) (^ (2^2) 1/2))) -> (* 3 (* 2 (^ 2 1/2)))
    ;; -> (* 6 (^ 2 1/2))
    (assert-equal (simplify '(^ 72 1/2)) '(* 6 (^ 2 1/2)))))
(define-test "simplify-power-integer-root-no-simplification"
  (lambda () (assert-equal (simplify '(^ 7 1/2)) '(^ 7 1/2))))
(define-test "simplify-power-integer-root-prime-base"
  (lambda () (assert-equal (simplify '(^ 5 1/3)) '(^ 5 1/3))))

;; Simplifying rational powers of positive integers
(define-test "simplify-power-integer-rational-exp-8_2_3"
  (lambda ()
    ;; (^ 8 2/3) -> (^ (2^3) 2/3) -> 2^(3*2/3) -> 2^2 -> 4
    (assert-equal (simplify '(^ 8 2/3)) 4)))
(define-test "simplify-power-integer-rational-exp-32_2_5"
  (lambda ()
    ;; (^ 32 2/5) -> (^ (2^5) 2/5) -> 2^(5*2/5) -> 2^2 -> 4
    (assert-equal (simplify '(^ 32 2/5)) 4)))
(define-test "simplify-power-integer-rational-exp-27_2_3"
  (lambda ()
    ;; (^ 27 2/3) -> (^ (3^3) 2/3) -> 3^(3*2/3) -> 3^2 -> 9
    (assert-equal (simplify '(^ 27 2/3)) 9)))
(define-test "simplify-power-integer-rational-exp-4_3_2"
  (lambda ()
    ;; (^ 4 3/2) -> (^ (2^2) 3/2) -> 2^(2*3/2) -> 2^3 -> 8
    (assert-equal (simplify '(^ 4 3/2)) 8)))
(define-test "simplify-power-integer-rational-exp-partially-simplifies"
  (lambda ()
    ;; (^ 12 3/2) -> (^ (* (2^2) 3) 3/2) -> (* (^ (2^2) 3/2) (^ 3 3/2))
    ;; -> (* (2^3) (^ 3 3/2)) -> (* 8 (^ 3 3/2))
    (assert-equal (simplify '(^ 12 3/2)) '(* 24 (^ 3 1/2)))))
(define-test "simplify-power-integer-rational-exp-already-simplified-form"
  (lambda () (assert-equal (simplify '(^ 5 2/3)) '(^ 5 2/3))))

;; Interaction with other power rules
(define-test "simplify-power-power-of-power-with-fractional"
  (lambda ()
    ;; (^ (^ x 6) 1/2) -> (^ x (* 6 1/2)) -> (^ x 3)
    (assert-equal (simplify '(^ (^ x 6) 1/2)) '(^ x 3))))
(define-test "simplify-power-power-of-power-with-fractional-inner"
  (lambda ()
    ;; (^ (^ x 1/2) 4) -> (^ x (* 1/2 4)) -> (^ x 2)
    (assert-equal (simplify '(^ (^ x 1/2) 4)) '(^ x 2))))
(define-test "simplify-power-power-of-product-with-fractional"
  (lambda ()
    ;; (^ (* x y) 1/2) -> (* (^ x 1/2) (^ y 1/2))
    (assert-equal (simplify '(^ (* x y) 1/2)) '(* (^ x 1/2) (^ y 1/2)))))

;; Edge cases for numeric root simplification
(define-test "simplify-power-base-one-fractional-exponent"
  (lambda () (assert-equal (simplify '(^ 1 1/2)) 1)))
(define-test "simplify-power-base-one-complex-fractional-exponent"
  (lambda () (assert-equal (simplify '(^ 1 7/13)) 1)))

;;; --- Tests for Basic Predicates ---
(define-test "predicate-constant?-1"
  (lambda () (assert-true (constant? 5))))
(define-test "predicate-constant?-2"
  (lambda () (assert-true (constant? -10))))
(define-test "predicate-constant?-3"
  (lambda () (assert-true (constant? 0))))
(define-test "predicate-constant?-4"
  (lambda () (assert-true (constant? 1/2))))
(define-test "predicate-constant?-5"
  (lambda () (assert-true (constant? -3/4))))
(define-test "predicate-constant?-6"
  (lambda () (assert-false (constant? 'x))))
(define-test "predicate-constant?-7"
  (lambda () (assert-false (constant? '(+ x 1)))))

(define-test "predicate-rational?-1"
  (lambda () (assert-true (rational? 5))))
(define-test "predicate-rational?-2"
  (lambda () (assert-true (rational? -10))))
(define-test "predicate-rational?-3"
  (lambda () (assert-true (rational? 0))))
(define-test "predicate-rational?-4"
  (lambda () (assert-true (rational? 1/2))))
(define-test "predicate-rational?-5"
  (lambda () (assert-true (rational? -3/4))))
(define-test "predicate-rational?-6"
  (lambda () (assert-false (rational? 'x))))
(define-test "predicate-rational?-7"
  (lambda () (assert-false (rational? '(+ x 1)))))
(define-test "predicate-rational?-8"
  (lambda () (assert-false (rational? "hello")))) ; Test with a string

(define-test "predicate-variable?-1"
  (lambda () (assert-true (variable? 'x))))
(define-test "predicate-variable?-2"
  (lambda () (assert-true (variable? 'my-var))))
(define-test "predicate-variable?-3"
  (lambda () (assert-false (variable? '+)))) ; Known operator
(define-test "predicate-variable?-4"
  (lambda () (assert-false (variable? 'sqrt)))) ; Known operator
(define-test "predicate-variable?-5"
  (lambda () (assert-false (variable? 5))))
(define-test "predicate-variable?-6"
  (lambda () (assert-false (variable? '(+ x 1)))))

;; Tests for (^ -1 rational-exponent)
(define-test "simplify-power-neg-one-half"
  (lambda () (assert-equal (simplify '(^ -1 1/2)) 'i)))
(define-test "simplify-power-neg-one-third"
  (lambda () (assert-equal (simplify '(^ -1 1/3)) -1)))
(define-test "simplify-power-neg-one-two-thirds"
  (lambda () (assert-equal (simplify '(^ -1 2/3)) 1)))
(define-test "simplify-power-neg-one-three-halves" ; (^ i 3)
  (lambda () (assert-equal (simplify '(^ -1 3/2)) '(- i))))
(define-test "simplify-power-neg-one-one-fourth" ; (^ i 1/2)
  (lambda () (assert-equal (simplify '(^ -1 1/4)) '(^ i 1/2))))
(define-test "simplify-power-neg-one-three-fourths" ; (^ i 3/2)
  (lambda () (assert-equal (simplify '(^ -1 3/4)) '(^ i 3/2))))

;; Tests for negative constant bases with rational exponents
(define-test "simplify-power-neg-base-sqrt-neg-four" ; (-4)^(1/2) -> (* (^ -1 1/2) (^ 4 1/2)) -> (* i 2)
  (lambda () (assert-equal (simplify '(^ -4 1/2)) '(* 2 i))))
(define-test "simplify-power-neg-base-sqrt-neg-nine"
  (lambda () (assert-equal (simplify '(^ -9 1/2)) '(* 3 i))))
(define-test "simplify-power-neg-base-cuberoot-neg-eight" ; (-8)^(1/3) -> -2
  (lambda () (assert-equal (simplify '(^ -8 1/3)) -2)))
(define-test "simplify-power-neg-base-cuberoot-neg-27"
  (lambda () (assert-equal (simplify '(^ -27 1/3)) -3)))
(define-test "simplify-power-neg-base-rational-exp-neg-8_2_3" ; (-8)^(2/3) -> ((-8)^(1/3))^2 -> (-2)^2 -> 4
  (lambda () (assert-equal (simplify '(^ -8 2/3)) 4)))
(define-test "simplify-power-neg-base-rational-exp-neg-4_3_2" ; (-4)^(3/2) -> (* (^ -1 3/2) (^ 4 3/2)) -> (* (- i) 8)
  (lambda () (assert-equal (simplify '(^ -4 3/2)) '(* -8 i)))) ; simplify-product might sort to this
(define-test "simplify-power-neg-base-fourthroot-neg-16" ; (-16)^(1/4) -> (* (^ -1 1/4) (^ 16 1/4)) -> (* (^ i 1/2) 2)
  (lambda () (assert-equal (simplify '(^ -16 1/4)) '(* 2 (^ i 1/2)))))
(define-test "simplify-power-neg-base-odd-den-even-num" ; e.g. (-2)^(2/3) -> (2)^(2/3)
  (lambda () (assert-equal (simplify '(^ -2 2/3)) '(^ 2 2/3))))
(define-test "simplify-power-neg-base-odd-den-odd-num-no-further-int-simpl" ; e.g. (-5)^(1/3) -> - (5^(1/3))
  (lambda () (assert-equal (simplify '(^ -5 1/3)) '(- (^ 5 1/3)))))

;;; --- Tests for abs Function ---
(define-test "simplify-abs-constant-positive"
  (lambda () (assert-equal (simplify '(abs 5)) 5)))
(define-test "simplify-abs-constant-negative"
  (lambda () (assert-equal (simplify '(abs -5)) 5)))
(define-test "simplify-abs-constant-zero"
  (lambda () (assert-equal (simplify '(abs 0)) 0)))
(define-test "simplify-abs-of-i"
  (lambda () (assert-equal (simplify '(abs i)) 1)))
(define-test "simplify-abs-of-negation"
  (lambda () (assert-equal (simplify '(abs (- x))) '(abs x))))
(define-test "simplify-abs-of-double-negation"
  (lambda () (assert-equal (simplify '(abs (- (- x)))) '(abs x))))
(define-test "simplify-abs-of-simplified-negation" ; abs(-(x+y)) -> abs(x+y)
  (lambda () (assert-equal (simplify '(abs (- (+ x y)))) '(abs (+ x y)))))
(define-test "simplify-abs-of-variable"
  (lambda () (assert-equal (simplify '(abs x)) '(abs x))))
(define-test "simplify-abs-of-sum"
  (lambda () (assert-equal (simplify '(abs (+ x y))) '(abs (+ x y)))))
(define-test "simplify-abs-power-even-exponent-var" ; abs(x^2) -> x^2
  (lambda () (assert-equal (simplify '(abs (^ x 2))) '(^ x 2))))
(define-test "simplify-abs-power-even-exponent-var-higher" ; abs(x^4) -> x^4
  (lambda () (assert-equal (simplify '(abs (^ x 4))) '(^ x 4))))
(define-test "simplify-abs-power-odd-exponent-var" ; abs(x^3) -> abs(x^3)
  (lambda () (assert-equal (simplify '(abs (^ x 3))) '(abs (^ x 3)))))
(define-test "simplify-abs-power-const-base-even-exp" ; abs((-2)^2) -> abs(4) -> 4
  (lambda () (assert-equal (simplify '(abs (^ -2 2))) 4)))
(define-test "simplify-abs-power-i-base-even-exp" ; abs(i^2) -> abs(-1) -> 1
  (lambda () (assert-equal (simplify '(abs (^ i 2))) 1)))
(define-test "simplify-abs-already-abs"
  (lambda () (assert-equal (simplify '(abs (abs x))) '(abs x)))) ; simplify-abs simplifies arg first

;;; --- Tests for Nth root of Nth power rule ---
(define-test "simplify-power-sqrt-x-squared" ; (^ (^ x 2) 1/2) -> (abs x)
  (lambda () (assert-equal (simplify '(^ (^ x 2) 1/2)) '(abs x))))
(define-test "simplify-power-fourthroot-x-fourth" ; (^ (^ x 4) 1/4) -> (abs x)
  (lambda () (assert-equal (simplify '(^ (^ x 4) 1/4)) '(abs x))))
(define-test "simplify-power-cuberoot-x-cubed" ; (^ (^ x 3) 1/3) -> x
  (lambda () (assert-equal (simplify '(^ (^ x 3) 1/3)) 'x)))
(define-test "simplify-power-sqrt-const-squared-pos" ; (^ (^ 3 2) 1/2) -> (abs 3) -> 3
  (lambda () (assert-equal (simplify '(^ (^ 3 2) 1/2)) 3)))
(define-test "simplify-power-sqrt-const-squared-neg" ; (^ (^ -3 2) 1/2) -> (abs -3) -> 3
  (lambda () (assert-equal (simplify '(^ (^ -3 2) 1/2)) 3)))
(define-test "simplify-power-cuberoot-const-cubed-neg" ; (^ (^ -2 3) 1/3) -> -2
  (lambda () (assert-equal (simplify '(^ (^ -2 3) 1/3)) -2)))
(define-test "simplify-power-sqrt-i-squared" ; (^ (^ i 2) 1/2) -> (abs i) -> 1
  (lambda () (assert-equal (simplify '(^ (^ i 2) 1/2)) 1)))
(define-test "simplify-power-fourthroot-i-fourth" ; (^ (^ i 4) 1/4) -> (abs i) -> 1
  (lambda () (assert-equal (simplify '(^ (^ i 4) 1/4)) 1)))
(define-test "simplify-power-sqrt-sum-squared" ; (^ (^ (+ x y) 2) 1/2) -> (abs (+ x y))
  (lambda () (assert-equal (simplify '(^ (^ (+ x y) 2) 1/2)) '(abs (+ x y)))))

;; Tests for (^ (base^M) (1/N)) -> (^ base (/ M N)) where M != N
(define-test "simplify-power-M-over-N-exponent-1" ; (^ (^ x 6) 1/3) -> (^ x 2)
  (lambda () (assert-equal (simplify '(^ (^ x 6) 1/3)) '(^ x 2))))

(define-test "simplify-power-M-over-N-exponent-2" ; (^ (^ x 2) 1/4) -> (^ x 1/2)
  (lambda () (assert-equal (simplify '(^ (^ x 2) 1/4)) '(^ x 1/2))))

(define-test "simplify-power-M-over-N-exponent-constants" ; (^ (^ 2 4) 1/2) -> (^ 2 2) -> 4
  (lambda () (assert-equal (simplify '(^ (^ 2 4) 1/2)) 4)))

(define-test "simplify-power-M-over-N-with-abs-base-no-abs-trigger" ; (^ (^ (abs y) 6) 1/3) -> (^ (abs y) 2)
  (lambda () (assert-equal (simplify '(^ (^ (abs y) 6) 1/3)) '(^ (abs y) 2))))

;;; --- Tests for exp Function ---
(define-test "simplify-exp-zero"
  (lambda () (assert-equal (simplify '(exp 0)) 1)))
(define-test "simplify-exp-of-ln" ; exp(ln(x)) -> x
  (lambda () (assert-equal (simplify '(exp (ln x))) 'x)))
(define-test "simplify-exp-of-ln-const" ; exp(ln(5)) -> 5
  (lambda () (assert-equal (simplify '(exp (ln 5))) 5)))
(define-test "simplify-exp-of-product-with-ln-k-first" ; exp(2 * ln(x)) -> x^2
  (lambda () (assert-equal (simplify '(exp (* 2 (ln x)))) '(^ x 2))))
(define-test "simplify-exp-of-product-with-ln-ln-first" ; exp(ln(x) * 2) -> x^2
  (lambda () (assert-equal (simplify '(exp (* (ln x) 2))) '(^ x 2))))
(define-test "simplify-exp-of-product-with-ln-symbolic-k" ; exp(y * ln(x)) -> x^y
  (lambda () (assert-equal (simplify '(exp (* y (ln x)))) '(^ x y))))
(define-test "simplify-exp-symbolic-arg"
  (lambda () (assert-equal (simplify '(exp x)) '(exp x))))
(define-test "simplify-exp-of-one-is-e" ; exp(1) -> e (This rule was not explicitly in README but is standard)
  (lambda () (assert-equal (simplify '(exp 1)) 'e)))


;;; --- Tests for ln Function ---
(define-test "simplify-ln-one"
  (lambda () (assert-equal (simplify '(ln 1)) 0)))
(define-test "simplify-ln-e"
  (lambda () (assert-equal (simplify '(ln e)) 1)))
(define-test "simplify-ln-of-exp" ; ln(exp(x)) -> x
  (lambda () (assert-equal (simplify '(ln (exp x))) 'x)))
(define-test "simplify-ln-of-exp-const" ; ln(exp(5)) -> 5
  (lambda () (assert-equal (simplify '(ln (exp 5))) 5)))
(define-test "simplify-ln-of-power" ; ln(x^2) -> 2*ln(x)
  (lambda () (assert-equal (simplify '(ln (^ x 2))) '(* 2 (ln x)))))
(define-test "simplify-ln-of-power-symbolic-exp" ; ln(x^y) -> y*ln(x)
  (lambda () (assert-equal (simplify '(ln (^ x y))) '(* y (ln x)))))
(define-test "simplify-ln-of-product" ; ln(x*y) -> ln(x) + ln(y)
  (lambda () (assert-equal (simplify '(ln (* x y))) '(+ (ln x) (ln y)))))
(define-test "simplify-ln-of-product-multiple" ; ln(x*y*z) -> ln(x) + ln(y) + ln(z)
  (lambda () (assert-equal (simplify '(ln (* x y z))) '(+ (ln x) (ln y) (ln z)))))
(define-test "simplify-ln-of-product-with-constant" ; ln(2*x) -> ln(2) + ln(x)
  (lambda () (assert-equal (simplify '(ln (* 2 x))) '(+ (ln 2) (ln x)))))
(define-test "simplify-ln-symbolic-arg"
  (lambda () (assert-equal (simplify '(ln x)) '(ln x))))
(define-test "simplify-ln-of-zero-in-product" ; ln(x*0) -> ln(0) - stays for now
  (lambda () (assert-equal (simplify '(ln (* x 0))) '(ln 0))))


;;; --- Tests for LaTeX rendering of exp and ln ---
(define-test "latex-render-exp-x"
  (lambda () (assert-equal (prefix-expr->markdown-latex '(exp x)) "$e^{x}$")))
(define-test "latex-render-ln-x"
  (lambda () (assert-equal (prefix-expr->markdown-latex '(ln x)) "$\\ln\\left(x\\right)$")))
(define-test "latex-render-exp-complex-arg"
  (lambda () (assert-equal (prefix-expr->markdown-latex '(exp (+ x 1))) "$e^{\\left(x + 1\\right)}$")))
(define-test "latex-render-ln-complex-arg"
  (lambda () (assert-equal (prefix-expr->markdown-latex '(ln (/ x 2))) "$\\ln\\left(\\frac{x}{2}\\right)$")))
(define-test "latex-render-constant-e"
  (lambda () (assert-equal (prefix-expr->markdown-latex 'e) "$e$")))

;;; --- Tests for Infix string rendering of exp and ln ---
(define-test "infix-render-exp-x"
  (lambda () (assert-equal (prefix-expr->infix-string '(exp x)) "exp(x)")))
(define-test "infix-render-ln-x"
  (lambda () (assert-equal (prefix-expr->infix-string '(ln x)) "ln(x)")))
(define-test "infix-render-constant-e"
  (lambda () (assert-equal (prefix-expr->infix-string 'e) "e")))


;;; --- Tests for sin Function ---
(define-test "simplify-sin-zero" (lambda () (assert-equal (simplify '(sin 0)) 0)))
(define-test "simplify-sin-pi" (lambda () (assert-equal (simplify '(sin pi)) 0)))
(define-test "simplify-sin-2pi" (lambda () (assert-equal (simplify '(sin (* 2 pi))) 0)))
(define-test "simplify-sin-pi-over-2" (lambda () (assert-equal (simplify '(sin (/ pi 2))) 1)))
(define-test "simplify-sin-pi-over-3" (lambda () (assert-equal (simplify '(sin (/ pi 3))) (simplify '(/ (^ 3 1/2) 2)))))
(define-test "simplify-sin-pi-over-4" (lambda () (assert-equal (simplify '(sin (/ pi 4))) (simplify '(/ (^ 2 1/2) 2)))))
(define-test "simplify-sin-pi-over-6" (lambda () (assert-equal (simplify '(sin (/ pi 6))) 1/2)))
(define-test "simplify-sin-neg-arg" (lambda () (assert-equal (simplify '(sin (- x))) '(- (sin x)))))
(define-test "simplify-sin-neg-pi-over-2" (lambda () (assert-equal (simplify '(sin (- (/ pi 2)))) -1)))
(define-test "simplify-sin-symbolic" (lambda () (assert-equal (simplify '(sin x)) '(sin x))))

;;; --- Tests for cos Function ---
(define-test "simplify-cos-zero" (lambda () (assert-equal (simplify '(cos 0)) 1)))
(define-test "simplify-cos-pi" (lambda () (assert-equal (simplify '(cos pi)) -1)))
(define-test "simplify-cos-2pi" (lambda () (assert-equal (simplify '(cos (* 2 pi))) 1)))
(define-test "simplify-cos-pi-over-2" (lambda () (assert-equal (simplify '(cos (/ pi 2))) 0)))
(define-test "simplify-cos-pi-over-3" (lambda () (assert-equal (simplify '(cos (/ pi 3))) 1/2)))
(define-test "simplify-cos-pi-over-4" (lambda () (assert-equal (simplify '(cos (/ pi 4))) (simplify '(/ (^ 2 1/2) 2)))))
(define-test "simplify-cos-pi-over-6" (lambda () (assert-equal (simplify '(cos (/ pi 6))) (simplify '(/ (^ 3 1/2) 2)))))
(define-test "simplify-cos-neg-arg" (lambda () (assert-equal (simplify '(cos (- x))) '(cos x))))
(define-test "simplify-cos-neg-pi" (lambda () (assert-equal (simplify '(cos (- pi))) -1)))
(define-test "simplify-cos-symbolic" (lambda () (assert-equal (simplify '(cos x)) '(cos x))))

;;; --- Tests for tan Function ---
(define-test "simplify-tan-zero" (lambda () (assert-equal (simplify '(tan 0)) 0)))
(define-test "simplify-tan-pi" (lambda () (assert-equal (simplify '(tan pi)) 0)))
(define-test "simplify-tan-pi-over-4" (lambda () (assert-equal (simplify '(tan (/ pi 4))) 1)))
(define-test "simplify-tan-pi-over-6" (lambda () (assert-equal (simplify '(tan (/ pi 6))) (simplify '(/ 1 (^ 3 1/2))))))
(define-test "simplify-tan-pi-over-3" (lambda () (assert-equal (simplify '(tan (/ pi 3))) (simplify '(^ 3 1/2)))))
(define-test "simplify-tan-pi-over-2-undefined" (lambda () (assert-equal (simplify '(tan (/ pi 2))) '(tan (/ pi 2)))))
(define-test "simplify-tan-neg-arg" (lambda () (assert-equal (simplify '(tan (- x))) '(- (tan x)))))
(define-test "simplify-tan-symbolic" (lambda () (assert-equal (simplify '(tan x)) '(tan x))))

;;; --- Tests for Trigonometric Identities ---
(define-test "simplify-pythagorean-identity-sin-cos"
  (lambda () (assert-equal (simplify '(+ (^ (sin x) 2) (^ (cos x) 2))) 1)))
(define-test "simplify-pythagorean-identity-cos-sin"
  (lambda () (assert-equal (simplify '(+ (^ (cos x) 2) (^ (sin x) 2))) 1)))
(define-test "simplify-pythagorean-identity-with-arg"
  (lambda () (assert-equal (simplify '(+ (^ (sin (+ x 1)) 2) (^ (cos (+ x 1)) 2))) 1)))
(define-test "simplify-tan-definition"
  (lambda () (assert-equal (simplify '(/ (sin x) (cos x))) '(tan x))))
(define-test "simplify-tan-definition-with-arg"
  (lambda () (assert-equal (simplify '(/ (sin (+ x y)) (cos (+ x y)))) '(tan (+ x y)))))

(define-test "simplify-tan-definition-evaluates-arg" ; e.g. tan(0)
  (lambda () (assert-equal (simplify '(/ (sin 0) (cos 0))) 0)))


;;; --- Tests for LaTeX rendering of sin, cos, tan, pi ---
(define-test "latex-render-sin-x" (lambda () (assert-equal (prefix-expr->markdown-latex '(sin x)) "$\\sin\\left(x\\right)$")))
(define-test "latex-render-cos-x" (lambda () (assert-equal (prefix-expr->markdown-latex '(cos x)) "$\\cos\\left(x\\right)$")))
(define-test "latex-render-tan-x" (lambda () (assert-equal (prefix-expr->markdown-latex '(tan x)) "$\\tan\\left(x\\right)$")))
(define-test "latex-render-pi" (lambda () (assert-equal (prefix-expr->markdown-latex 'pi) "$\\pi$")))
(define-test "latex-render-sin-pi-over-2" (lambda () (assert-equal (prefix-expr->markdown-latex '(sin (/ pi 2))) "$\\sin\\left(\\frac{\\pi}{2}\\right)$")))

;;; --- Tests for Infix string rendering of sin, cos, tan, pi ---
(define-test "infix-render-sin-x" (lambda () (assert-equal (prefix-expr->infix-string '(sin x)) "sin(x)")))
(define-test "infix-render-cos-x" (lambda () (assert-equal (prefix-expr->infix-string '(cos x)) "cos(x)")))
(define-test "infix-render-tan-x" (lambda () (assert-equal (prefix-expr->infix-string '(tan x)) "tan(x)")))
(define-test "infix-render-pi" (lambda () (assert-equal (prefix-expr->infix-string 'pi) "pi")))

(define-test "expand-sin-A-plus-B"
  (lambda ()
    ;; sin(A+B) -> sin(A)cos(B)+cos(A)sin(B)
    ;; Expected form after simplify will sort terms and factors.
    ;; Assuming (sin A) < (cos A) and (sin B) < (cos B) by term<? on their string forms,
    ;; and A < B.
    ;; Product terms: (* (cos B) (sin A)) and (* (cos A) (sin B))
    ;; simplify will order these terms in the sum.
    (assert-equal (expand '(sin (+ A B)))
                (simplify '(+ (* (sin A) (cos B)) (* (cos A) (sin B)))))))

(define-test "expand-sin-A-minus-B"
  (lambda ()
    ;; sin(A-B) -> sin(A)cos(B)-cos(A)sin(B)
    ;; expand '(sin (- A B)) becomes expand '(sin (+ A (- B)))
    ;; -> sin(A)cos(-B) + cos(A)sin(-B)
    ;; -> sin(A)cos(B) - cos(A)sin(B)
    (assert-equal (expand '(sin (- A B)))
                (simplify '(+ (* (sin A) (cos B)) (* -1 (cos A) (sin B)))))))

(define-test "expand-cos-A-plus-B"
  (lambda ()
    ;; cos(A+B) -> cos(A)cos(B)-sin(A)sin(B)
    (assert-equal (expand '(cos (+ A B)))
                (simplify '(+ (* (cos A) (cos B)) (* -1 (sin A) (sin B)))))))

(define-test "expand-cos-A-minus-B"
  (lambda ()
    ;; cos(A-B) -> cos(A)cos(B)+sin(A)sin(B)
    ;; expand '(cos (- A B)) becomes expand '(cos (+ A (- B)))
    ;; -> cos(A)cos(-B) - sin(A)sin(-B)
    ;; -> cos(A)cos(B) + sin(A)sin(B)
    (assert-equal (expand '(cos (- A B)))
                (simplify '(+ (* (cos A) (cos B)) (* (sin A) (sin B)))))))

(define-test "expand-sin-2A"
  (lambda ()
    ;; sin(2A) -> 2sin(A)cos(A)
    (assert-equal (expand '(sin (* 2 A)))
                (simplify '(* 2 (sin A) (cos A))))))

(define-test "expand-cos-2A"
  (lambda ()
    ;; cos(2A) -> cos^2(A)-sin^2(A)
    (assert-equal (expand '(cos (* 2 A)))
                (simplify '(+ (^ (cos A) 2) (* -1 (^ (sin A) 2)))))))

(define-test "expand-cos-2x"
  (lambda () (assert-equal (expand '(cos (* 2 x))) '(+ (- (^ (sin x) 2)) (^ (cos x) 2)))))

;; Tests for tan expansions
(define-test "expand-tan-x-plus-y"
  (lambda ()
    (assert-equal (expand '(tan (+ x y)))
                  '(/ (+ (tan x) (tan y)) (+ 1 (- (* (tan x) (tan y))))))))

(define-test "expand-tan-2x"
  (lambda ()
    (assert-equal (expand '(tan (* 2 x)))
                  '(/ (* 2 (tan x)) (+ 1 (- (^ (tan x) 2)))))))

(define-test "expand-tan-of-product-with-constant"
  (lambda ()
    (assert-equal (expand '(tan (* 2 pi))) ; tan(2pi) simplifies to 0
                  0)))

(define-test "expand-tan-already-simple"
  (lambda () (assert-equal (expand '(tan x)) '(tan x))))

(define-test "expand-tan-arg-is-atomic"
  (lambda () (assert-equal (expand '(tan x)) '(tan x))))

;; Contraction Test (using `simplify`)

(define-test "simplify-2sinAcosA-to-sin2A"
  (lambda ()
    ;; 2sin(A)cos(A) -> sin(2A)
    (assert-equal (simplify '(* 2 (sin A) (cos A)))
                '(sin (* 2 A)))))

(define-test "simplify-2cosAsinA-to-sin2A"
  (lambda ()
    ;; 2cos(A)sin(A) -> sin(2A) (order of sin/cos factors)
    (assert-equal (simplify '(* 2 (cos A) (sin A)))
                '(sin (* 2 A)))))

(define-test "simplify-sinA2cosA-to-sin2A"
  (lambda ()
    ;; sin(A)*2*cos(A) -> sin(2A) (order of 2)
    (assert-equal (simplify '(* (sin A) 2 (cos A)))
                '(sin (* 2 A)))))

(define-test "simplify-product-with-other-factors-2sinAcosA"
  (lambda ()
    ;; x * 2 * sin(A) * cos(A) * y -> x * y * sin(2A)
    (assert-equal (simplify '(* x 2 (sin A) (cos A) y))
                (simplify '(* x y (sin (* 2 A)))))))
