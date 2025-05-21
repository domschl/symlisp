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
