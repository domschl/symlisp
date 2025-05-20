;;;-----------------------------------------------------------------------------
;;; Infix Tokenizer Tests (Revised based on cleaner C logic and correct assert order)
;;;-----------------------------------------------------------------------------

(define-test "infix-tokens-simple-add"
  (lambda () (assert-equal (string->infix-tokens "1 + 2") '("1" "+" "2"))))

(define-test "infix-tokens-simple-var"
  (lambda () (assert-equal (string->infix-tokens "a * b") '("a" "*" "b"))))

(define-test "infix-tokens-mixed"
  (lambda () (assert-equal (string->infix-tokens "x^2-3*x") '("x" "^" "2" "-" "3" "*" "x"))))

(define-test "infix-tokens-parens"
  (lambda () (assert-equal (string->infix-tokens "(a+b)*c") '("(" "a" "+" "b" ")" "*" "c"))))

(define-test "infix-tokens-function"
  (lambda () (assert-equal (string->infix-tokens "f(x,y)") '("f" "(" "x" "," "y" ")"))))

(define-test "infix-tokens-leading-space"
  (lambda () (assert-equal (string->infix-tokens "  1 + 2") '("1" "+" "2"))))

(define-test "infix-tokens-trailing-space"
  (lambda () (assert-equal (string->infix-tokens "1 + 2  ") '("1" "+" "2"))))

(define-test "infix-tokens-internal-space"
  (lambda () (assert-equal (string->infix-tokens "1  +   2") '("1" "+" "2"))))

(define-test "infix-tokens-no-space"
  (lambda () (assert-equal (string->infix-tokens "1+2*x") '("1" "+" "2" "*" "x"))))

(define-test "infix-tokens-unary-minus-start"
  (lambda () (assert-equal (string->infix-tokens "-x") '("-" "x"))))

(define-test "infix-tokens-unary-minus-expr"
  (lambda () (assert-equal (string->infix-tokens "a+(-b)") '("a" "+" "(" "-" "b" ")"))))

(define-test "infix-tokens-unary-plus-start"
  (lambda () (assert-equal (string->infix-tokens "+5") '("+" "5")))) ; Note: "+" and "5" are separate strings

(define-test "infix-tokens-decimal"
  (lambda () (assert-equal (string->infix-tokens "3.14*r^2") '("3.14" "*" "r" "^" "2"))))

(define-test "infix-tokens-multi-char-op"
  (lambda () (assert-equal (string->infix-tokens "x<=y") '("x" "<=" "y"))))

(define-test "infix-tokens-complex"
  (lambda () (assert-equal (string->infix-tokens "x^2-3*x-g(x)+1/(x+1)")
                '("x" "^" "2" "-" "3" "*" "x" "-" "g" "(" "x" ")" "+" "1" "/" "(" "x" "+" "1" ")"))))

(define-test "infix-tokens-empty"
  (lambda () (assert-equal (string->infix-tokens "") '())))

(define-test "infix-tokens-whitespace-only"
  (lambda () (assert-equal (string->infix-tokens "   \t \n  ") '())))

;; Error tests might need adjustment depending on C implementation
;(define-test "infix-tokens-error-char" ...)
;(define-test "infix-tokens-error-bad-num" ...) // This error wouldn't happen in the tokenizer anymore

;;; --- high-level tests for infix to prefix conversion ---
(define-test "infix-to-prefix-simple-add"
  (lambda () (assert-equal (string->prefix-expr "1 + 2") '(+ 1 2))))
(define-test "infix-to-prefix-simple-var"
  (lambda () (assert-equal (string->prefix-expr "a * b") '(* a b))))
(define-test "infix-to-prefix-mixed"
  (lambda () (assert-equal (string->prefix-expr "x^2-3*x") '(- (^ x 2) (* 3 x)))))
(define-test "infix-to-prefix-parens"
  (lambda () (assert-equal (string->prefix-expr "(a+b)*c") '(* (+ a b) c))))
(define-test "right-associativity"
  (lambda () (assert-equal (string->prefix-expr "x^y^z") '(^ x (^ y z)))))
(define-test "left-associativity"
  (lambda () (assert-equal (string->prefix-expr "x-y+z") '(+ (- x y) z))))
  
(define-test "string->prefix-1" 
  (lambda () (assert-equal (string->prefix-expr "1 + 2") '(+ 1 2))))

(define-test "string->prefix-2"
  (lambda () (assert-equal (string->prefix-expr "a * b") '(* a b))))
(define-test "string->prefix-3"
  (lambda () (assert-equal (string->prefix-expr "1 + 2 * 3") '(+ 1 (* 2 3)))))
(define-test "string->prefix-4"
  (lambda () (assert-equal (string->prefix-expr "(1 + 2) * 3") '(* (+ 1 2) 3))))
(define-test "string->prefix-5"
  (lambda () (assert-equal (string->prefix-expr "a ^ b ^ c") '(^ a (^ b c))))) ; right associative
(define-test "string->prefix-6"
  (lambda () (assert-equal (string->prefix-expr "3 - 4 + 5") '(+ (- 3 4) 5)))) ; left associative
(define-test "string->prefix-math"
  (lambda () (assert-equal (eval (string->prefix-expr "3*(4+1)")) 15)))

(define-test "string->prefix-7"
  (lambda () (assert-equal (string->prefix-expr "f(x) + g(x,y)") '(+ (f x) (g x y)))))
(define-test "string->prefix-8"
  (lambda () (assert-equal (string->prefix-expr "f()") '(f))))
(define-test "string->prefix-9"
  (lambda () (assert-equal (string->prefix-expr "f(a,b*c,d)") '(f a (* b c) d))))
(define-test "string->prefix-10"
  (lambda () (assert-equal (string->prefix-expr "-a * (b + -c)") '(* (- a) (+ b (- c))))))

;; --- More Complex Test Cases ---

(define-test "string->prefix-complex-1-nested-func"
  (lambda () (assert-equal (string->prefix-expr "f(g(x), y)") '(f (g x) y))))

(define-test "string->prefix-complex-2-op-in-args"
  (lambda () (assert-equal (string->prefix-expr "f(x + 1, y * 2)") '(f (+ x 1) (* y 2)))))

(define-test "string->prefix-complex-3-unary-in-args"
  (lambda () (assert-equal (string->prefix-expr "f(-x, -1)") '(f (- x) (- 1)))))

(define-test "string->prefix-complex-4-mixed-ops-and-funcs"
  (lambda () (assert-equal (string->prefix-expr "a + f(b * c) - d") '(- (+ a (f (* b c))) d))))

(define-test "string->prefix-complex-5-func-with-no-args-and-ops"
  (lambda () (assert-equal (string->prefix-expr "1 + f() * 2") '(+ 1 (* (f) 2)))))

(define-test "string->prefix-complex-6-deeply-nested"
  (lambda () (assert-equal (string->prefix-expr "h(f(a+b, g(c*d)), -e)") '(h (f (+ a b) (g (* c d))) (- e)))))

(define-test "string->prefix-complex-7-unary-and-binary-minus"
  (lambda () (assert-equal (string->prefix-expr "-x - -y") '(- (- x) (- y)))))

(define-test "string->prefix-complex-8-power-and-func"
  (lambda () (assert-equal (string->prefix-expr "f(x)^2 + g(y)") '(+ (^ (f x) 2) (g y)))))

(define-test "string->prefix-complex-9-multiple-funcs-and-ops"
  (lambda () (assert-equal (string->prefix-expr "f(a) * g(b) + h(c) / d") '(+ (* (f a) (g b)) (/ (h c) d)))))

(define-test "string->prefix-complex-10-func-arg-is-complex-expr"
  (lambda () (assert-equal (string->prefix-expr "f( (a+b)*(c-d) / e )") '(f (/ (* (+ a b) (- c d)) e)))))

(define-test "string->prefix-complex-11-unary-on-parenthesized-expr"
  (lambda () (assert-equal (string->prefix-expr "-(a+b)") '(- (+ a b)))))

(define-test "string->prefix-complex-12-unary-on-function-result"
  (lambda () (assert-equal (string->prefix-expr "-f(x) + g(y)") '(+ (- (f x)) (g y)))))

(define-test "expr->string-simple-add"
  (lambda () (assert-equal (prefix-expr->infix-string '(+ 1 2)) "1 + 2")))

(define-test "expr->string-simple-vars"
  (lambda () (assert-equal (prefix-expr->infix-string '(* a b)) "a * b")))

(define-test "expr->string-precedence-1"
  (lambda () (assert-equal (prefix-expr->infix-string '(+ 1 (* 2 3))) "1 + 2 * 3")))

(define-test "expr->string-precedence-2"
  (lambda () (assert-equal (prefix-expr->infix-string '(* (+ 1 2) 3)) "(1 + 2) * 3")))

(define-test "expr->string-left-assoc-minus"
  (lambda () (assert-equal (prefix-expr->infix-string '(- (- a b) c)) "a - b - c"))) ; (a-b)-c

(define-test "expr->string-left-assoc-plus-minus"
  (lambda () (assert-equal (prefix-expr->infix-string '(+ (- a b) c)) "a - b + c")))

(define-test "expr->string-right-assoc-power"
  (lambda () (assert-equal (prefix-expr->infix-string '(^ a (^ b c))) "a ^ b ^ c"))) ; a^(b^c)

(define-test "expr->string-power-precedence"
  (lambda () (assert-equal (prefix-expr->infix-string '(* (^ a b) c)) "a ^ b * c")))

(define-test "expr->string-power-precedence-rhs"
  (lambda () (assert-equal (prefix-expr->infix-string '(* c (^ a b))) "c * a ^ b")))

(define-test "expr->string-unary-minus-simple"
  (lambda () (assert-equal (prefix-expr->infix-string '(- x)) "-x")))

(define-test "expr->string-unary-minus-in-expr"
  (lambda () (assert-equal (prefix-expr->infix-string '(+ a (- b))) "a + -b")))

(define-test "expr->string-unary-minus-higher-precedence"
  (lambda () (assert-equal (prefix-expr->infix-string '(* a (- b))) "a * -b")))

(define-test "expr->string-double-unary-minus"
  (lambda () (assert-equal (prefix-expr->infix-string '(- (- x))) "-(-x)"))) ; or "--x" if unary minus op char is just "-"

(define-test "expr->string-function-simple"
  (lambda () (assert-equal (prefix-expr->infix-string '(f x y)) "f(x, y)")))

(define-test "expr->string-function-no-args"
  (lambda () (assert-equal (prefix-expr->infix-string '(g)) "g()")))

(define-test "expr->string-function-nested"
  (lambda () (assert-equal (prefix-expr->infix-string '(f (g x) y)) "f(g(x), y)")))

(define-test "expr->string-function-with-expr-args"
  (lambda () (assert-equal (prefix-expr->infix-string '(f (+ x 1) (* y z))) "f(x + 1, y * z)")))

(define-test "expr->string-variadic-plus"
  (lambda () (assert-equal (prefix-expr->infix-string '(+ a b c d)) "a + b + c + d")))

(define-test "expr->string-variadic-times"
  (lambda () (assert-equal (prefix-expr->infix-string '(* a b c d)) "a * b * c * d")))

(define-test "expr->string-variadic-plus-times-mixed"
  (lambda () (assert-equal (prefix-expr->infix-string '(+ a (* b c) d (* e f g))) "a + b * c + d + e * f * g")))

(define-test "expr->string-complex-from-expand"
  ;; (+ (* 3 a (^ x 2)) (* 3 x (^ a 2)) (^ a 3) (^ x 3))
  ;; Expected: "3 * a * x ^ 2 + 3 * x * a ^ 2 + a ^ 3 + x ^ 3" (order might vary based on term<? if simplify was involved)
  ;; For direct conversion without prior sort:
  (lambda () (assert-equal (prefix-expr->infix-string '(+ (* 3 a (^ x 2)) (* 3 x (^ a 2)) (^ a 3) (^ x 3)))
                "3 * a * x ^ 2 + 3 * x * a ^ 2 + a ^ 3 + x ^ 3")))

(define-test "expr->string-division"
  (lambda () (assert-equal (prefix-expr->infix-string '(/ (+ a b) c)) "(a + b) / c")))

(define-test "expr->string-unary-plus-identity" ; Assuming (+ x) prints as x
  (lambda () (assert-equal (prefix-expr->infix-string '(+ x)) "x")))

(define-test "expr->string-unary-times-identity" ; Assuming (* x) prints as x
  (lambda () (assert-equal (prefix-expr->infix-string '(* x)) "x")))  


;;;-----------------------------------------------------------------------------
;;; Tests for prefix-expr->markdown-latex (Expression to LaTeX String)
;;;-----------------------------------------------------------------------------

(define-test "expr->latex-simple-add"
  (lambda () (assert-equal (prefix-expr->markdown-latex '(+ 1 2)) "$1 + 2$")))

(define-test "expr->latex-simple-vars"
  (lambda () (assert-equal (prefix-expr->markdown-latex '(* a b)) "$a \\cdot b$")))

(define-test "expr->latex-precedence-1"
  (lambda () (assert-equal (prefix-expr->markdown-latex '(+ 1 (* 2 3))) "$1 + 2 \\cdot 3$")))

(define-test "expr->latex-precedence-2"
  (lambda () (assert-equal (prefix-expr->markdown-latex '(* (+ 1 2) 3)) "$\\left(1 + 2\\right) \\cdot 3$")))

(define-test "expr->latex-division"
  (lambda () (assert-equal (prefix-expr->markdown-latex '(/ x 2)) "$\\frac{x}{2}$")))

(define-test "expr->latex-division-complex-num"
  (lambda () (assert-equal (prefix-expr->markdown-latex '(/ (+ x y) 2)) "$\\frac{x + y}{2}$")))

(define-test "expr->latex-division-complex-den"
  (lambda () (assert-equal (prefix-expr->markdown-latex '(/ x (+ y 2))) "$\\frac{x}{y + 2}$")))

(define-test "expr->latex-power"
  (lambda () (assert-equal (prefix-expr->markdown-latex '(^ x 2)) "$x^{2}$")))

(define-test "expr->latex-power-complex-base"
  (lambda () (assert-equal (prefix-expr->markdown-latex '(^ (+ x y) 2)) "$\\left(x + y\\right)^{2}$")))

(define-test "expr->latex-power-complex-exp"
  (lambda () (assert-equal (prefix-expr->markdown-latex '(^ x (+ y 2))) "$x^{y + 2}$")))
  
(define-test "expr->latex-power-right-assoc"
  (lambda () (assert-equal (prefix-expr->markdown-latex '(^ x (^ y z))) "$x^{y^{z}}$")))

(define-test "expr->latex-unary-minus-simple"
  (lambda () (assert-equal (prefix-expr->markdown-latex '(- x)) "$-x$")))

(define-test "expr->latex-unary-minus-in-expr"
  (lambda () (assert-equal (prefix-expr->markdown-latex '(+ a (- b))) "$a + -b$"))) ; or "$a - b$" if (+ a (-b)) simplifies first

(define-test "expr->latex-function-sin"
  (lambda () (assert-equal (prefix-expr->markdown-latex '(sin x)) "$\\sin\\left(x\\right)$")))

(define-test "expr->latex-function-cos-expr"
  (lambda () (assert-equal (prefix-expr->markdown-latex '(cos (+ x pi))) "$\\cos\\left(x + \\pi\\right)$")))

(define-test "expr->latex-function-sqrt"
  (lambda () (assert-equal (prefix-expr->markdown-latex '(sqrt (+ (^ x 2) (^ y 2)))) "$\\sqrt{x^{2} + y^{2}}$")))

(define-test "expr->latex-variadic-plus"
  (lambda () (assert-equal (prefix-expr->markdown-latex '(+ a b c d)) "$a + b + c + d$")))

(define-test "expr->latex-variadic-times"
  (lambda () (assert-equal (prefix-expr->markdown-latex '(* a b c d)) "$a \\cdot b \\cdot c \\cdot d$")))

(define-test "expr->latex-complex-from-expand"
  ;; (+ (* 3 a (^ x 2)) (* 3 x (^ a 2)) (^ a 3) (^ x 3))
  (lambda () (assert-equal (prefix-expr->markdown-latex '(+ (* 3 a (^ x 2)) (* 3 x (^ a 2)) (^ a 3) (^ x 3)))
                "$3 \\cdot a \\cdot x^{2} + 3 \\cdot x \\cdot a^{2} + a^{3} + x^{3}$")))

(define-test "expr->latex-sin-squared-x"
  (lambda () (assert-equal (prefix-expr->markdown-latex '(^ (sin x) 2)) "$\\sin^{2}\\left(x\\right)$")))

(define-test "expr->latex-log-base-exp"
  (lambda () (assert-equal (prefix-expr->markdown-latex '(log (^ x 2))) "$\\log\\left(x^{2}\\right)$")))

(define-test "expr->latex-unary-plus-identity" ; Assuming (+ x) prints as x
  (lambda () (assert-equal (prefix-expr->markdown-latex '(+ x)) "$x$")))

(define-test "expr->latex-unary-times-identity" ; Assuming (* x) prints as x
  (lambda () (assert-equal (prefix-expr->markdown-latex '(* x)) "$x$")))
