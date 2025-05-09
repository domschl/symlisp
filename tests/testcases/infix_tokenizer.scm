;;;-----------------------------------------------------------------------------
;;; Infix Tokenizer Tests (Revised based on cleaner C logic and correct assert order)
;;;-----------------------------------------------------------------------------

(define-test "infix-tokens-simple-add"
  (assert-equal (string->infix-tokens "1 + 2") '("1" "+" "2")))

(define-test "infix-tokens-simple-var"
  (assert-equal (string->infix-tokens "a * b") '("a" "*" "b")))

(define-test "infix-tokens-mixed"
  (assert-equal (string->infix-tokens "x^2-3*x") '("x" "^" "2" "-" "3" "*" "x")))

(define-test "infix-tokens-parens"
  (assert-equal (string->infix-tokens "(a+b)*c") '("(" "a" "+" "b" ")" "*" "c")))

(define-test "infix-tokens-function"
  (assert-equal (string->infix-tokens "f(x,y)") '("f" "(" "x" "," "y" ")")))

(define-test "infix-tokens-leading-space"
  (assert-equal (string->infix-tokens "  1 + 2") '("1" "+" "2")))

(define-test "infix-tokens-trailing-space"
  (assert-equal (string->infix-tokens "1 + 2  ") '("1" "+" "2")))

(define-test "infix-tokens-internal-space"
  (assert-equal (string->infix-tokens "1  +   2") '("1" "+" "2")))

(define-test "infix-tokens-no-space"
  (assert-equal (string->infix-tokens "1+2*x") '("1" "+" "2" "*" "x")))

(define-test "infix-tokens-unary-minus-start"
  (assert-equal (string->infix-tokens "-x") '("-" "x")))

(define-test "infix-tokens-unary-minus-expr"
  (assert-equal (string->infix-tokens "a+(-b)") '("a" "+" "(" "-" "b" ")")))

(define-test "infix-tokens-unary-plus-start"
  (assert-equal (string->infix-tokens "+5") '("+" "5"))) ; Note: "+" and "5" are separate strings

(define-test "infix-tokens-decimal"
  (assert-equal (string->infix-tokens "3.14*r^2") '("3.14" "*" "r" "^" "2")))

(define-test "infix-tokens-multi-char-op"
  (assert-equal (string->infix-tokens "x<=y") '("x" "<=" "y")))

(define-test "infix-tokens-complex"
  (assert-equal (string->infix-tokens "x^2-3*x-g(x)+1/(x+1)")
                '("x" "^" "2" "-" "3" "*" "x" "-" "g" "(" "x" ")" "+" "1" "/" "(" "x" "+" "1" ")")))

(define-test "infix-tokens-empty"
  (assert-equal (string->infix-tokens "") '()))

(define-test "infix-tokens-whitespace-only"
  (assert-equal (string->infix-tokens "   \t \n  ") '()))

;; Error tests might need adjustment depending on C implementation
;(define-test "infix-tokens-error-char" ...)
;(define-test "infix-tokens-error-bad-num" ...) // This error wouldn't happen in the tokenizer anymore

;;; --- high-level tests for infix to prefix conversion ---
(define-test "infix-to-prefix-simple-add"
  (assert-equal (string->prefix-expr "1 + 2") '(+ 1 2)))
(define-test "infix-to-prefix-simple-var"
  (assert-equal (string->prefix-expr "a * b") '(* a b)))
(define-test "infix-to-prefix-mixed"
  (assert-equal (string->prefix-expr "x^2-3*x") '(- (^ x 2) (* 3 x))))
(define-test "infix-to-prefix-parens"
  (assert-equal (string->prefix-expr "(a+b)*c") '(* (+ a b) c)))
(define-test "right-associativity"
  (assert-equal (string->prefix-expr "x^y^z") '(^ x (^ y z))))
(define-test "left-associativity"
  (assert-equal (string->prefix-expr "x-y+z") '(+ (- x y) z)))
  
(define-test "string->prefix-1" 
  (assert-equal (string->prefix-expr "1 + 2")  '(+ 1 2)))

(define-test "string->prefix-2"
  (assert-equal (string->prefix-expr "a * b")  '(* a b)))
(define-test "string->prefix-3"
  (assert-equal (string->prefix-expr "1 + 2 * 3")  '(+ 1 (* 2 3))))
(define-test "string->prefix-4"
  (assert-equal (string->prefix-expr "(1 + 2) * 3")  '(* (+ 1 2) 3)))
(define-test "string->prefix-5"
  (assert-equal (string->prefix-expr "a ^ b ^ c")  '(^ a (^ b c)))) ; right associative
(define-test "string->prefix-6"
  (assert-equal (string->prefix-expr "3 - 4 + 5")  '(+ (- 3 4) 5))) ; left associative
(define-test "string->prefix-math"
  (assert-equal (eval (string->prefix-expr "3*(4+1)")) 15))

(define-test "string->prefix-7"
  (assert-equal (string->prefix-expr "f(x) + g(x,y)") '(+ (f x) (g x y))))
(define-test "string->prefix-8"
  (assert-equal (string->prefix-expr "f()") '(f)))
(define-test "string->prefix-9"
  (assert-equal (string->prefix-expr "f(a,b*c,d)") '(f a (* b c) d)))
(define-test "string->prefix-10"
  (assert-equal (string->prefix-expr "-a * (b + -c)") '(* (- a) (+ b (- c)))))

;; --- More Complex Test Cases ---

(define-test "string->prefix-complex-1-nested-func"
  (assert-equal (string->prefix-expr "f(g(x), y)") '(f (g x) y)))

(define-test "string->prefix-complex-2-op-in-args"
  (assert-equal (string->prefix-expr "f(x + 1, y * 2)") '(f (+ x 1) (* y 2))))

(define-test "string->prefix-complex-3-unary-in-args"
  (assert-equal (string->prefix-expr "f(-x, -1)") '(f (- x) (- 1))))

(define-test "string->prefix-complex-4-mixed-ops-and-funcs"
  (assert-equal (string->prefix-expr "a + f(b * c) - d") '(- (+ a (f (* b c))) d)))

(define-test "string->prefix-complex-5-func-with-no-args-and-ops"
  (assert-equal (string->prefix-expr "1 + f() * 2") '(+ 1 (* (f) 2))))

(define-test "string->prefix-complex-6-deeply-nested"
  (assert-equal (string->prefix-expr "h(f(a+b, g(c*d)), -e)") '(h (f (+ a b) (g (* c d))) (- e))))

(define-test "string->prefix-complex-7-unary-and-binary-minus"
  (assert-equal (string->prefix-expr "-x - -y") '(- (- x) (- y))))

(define-test "string->prefix-complex-8-power-and-func"
  (assert-equal (string->prefix-expr "f(x)^2 + g(y)") '(+ (^ (f x) 2) (g y))))

(define-test "string->prefix-complex-9-multiple-funcs-and-ops"
  (assert-equal (string->prefix-expr "f(a) * g(b) + h(c) / d") '(+ (* (f a) (g b)) (/ (h c) d))))

(define-test "string->prefix-complex-10-func-arg-is-complex-expr"
  (assert-equal (string->prefix-expr "f( (a+b)*(c-d) / e )") '(f (/ (* (+ a b) (- c d)) e))))

(define-test "string->prefix-complex-11-unary-on-parenthesized-expr"
  (assert-equal (string->prefix-expr "-(a+b)") '(- (+ a b))))

(define-test "string->prefix-complex-12-unary-on-function-result"
  (assert-equal (string->prefix-expr "-f(x) + g(y)") '(+ (- (f x)) (g y))))

;; --- More Complex Test Cases ---

(define-test "string->prefix-complex-1-nested-func"
  (assert-equal (string->prefix-expr "f(g(x), y)") '(f (g x) y)))

(define-test "string->prefix-complex-2-op-in-args"
  (assert-equal (string->prefix-expr "f(x + 1, y * 2)") '(f (+ x 1) (* y 2))))

(define-test "string->prefix-complex-3-unary-in-args"
  (assert-equal (string->prefix-expr "f(-x, -1)") '(f (- x) (- 1))))

(define-test "string->prefix-complex-4-mixed-ops-and-funcs"
  (assert-equal (string->prefix-expr "a + f(b * c) - d") '(- (+ a (f (* b c))) d)))

(define-test "string->prefix-complex-5-func-with-no-args-and-ops"
  (assert-equal (string->prefix-expr "1 + f() * 2") '(+ 1 (* (f) 2))))

(define-test "string->prefix-complex-6-deeply-nested"
  (assert-equal (string->prefix-expr "h(f(a+b, g(c*d)), -e)") '(h (f (+ a b) (g (* c d))) (- e))))

(define-test "string->prefix-complex-7-unary-and-binary-minus"
  (assert-equal (string->prefix-expr "-x - -y") '(- (- x) (- y))))

(define-test "string->prefix-complex-8-power-and-func"
  (assert-equal (string->prefix-expr "f(x)^2 + g(y)") '(+ (^ (f x) 2) (g y))))

(define-test "string->prefix-complex-9-multiple-funcs-and-ops"
  (assert-equal (string->prefix-expr "f(a) * g(b) + h(c) / d") '(+ (* (f a) (g b)) (/ (h c) d))))

(define-test "string->prefix-complex-10-func-arg-is-complex-expr"
  (assert-equal (string->prefix-expr "f( (a+b)*(c-d) / e )") '(f (/ (* (+ a b) (- c d)) e))))

(define-test "string->prefix-complex-11-unary-on-parenthesized-expr"
  (assert-equal (string->prefix-expr "-(a+b)") '(- (+ a b))))

(define-test "string->prefix-complex-12-unary-on-function-result"
  (assert-equal (string->prefix-expr "-f(x) + g(y)") '(+ (- (f x)) (g y))))

(define-test "expr->string-simple-add"
  (assert-equal (prefix-expr->string '(+ 1 2)) "1 + 2"))

(define-test "expr->string-simple-vars"
  (assert-equal (prefix-expr->string '(* a b)) "a * b"))

(define-test "expr->string-precedence-1"
  (assert-equal (prefix-expr->string '(+ 1 (* 2 3))) "1 + 2 * 3"))

(define-test "expr->string-precedence-2"
  (assert-equal (prefix-expr->string '(* (+ 1 2) 3)) "(1 + 2) * 3"))

(define-test "expr->string-left-assoc-minus"
  (assert-equal (prefix-expr->string '(- (- a b) c)) "a - b - c")) ; (a-b)-c

(define-test "expr->string-left-assoc-plus-minus"
  (assert-equal (prefix-expr->string '(+ (- a b) c)) "a - b + c"))

(define-test "expr->string-right-assoc-power"
  (assert-equal (prefix-expr->string '(^ a (^ b c))) "a ^ b ^ c")) ; a^(b^c)

(define-test "expr->string-power-precedence"
  (assert-equal (prefix-expr->string '(* (^ a b) c)) "a ^ b * c"))

(define-test "expr->string-power-precedence-rhs"
  (assert-equal (prefix-expr->string '(* c (^ a b))) "c * a ^ b"))

(define-test "expr->string-unary-minus-simple"
  (assert-equal (prefix-expr->string '(- x)) "-x"))

(define-test "expr->string-unary-minus-in-expr"
  (assert-equal (prefix-expr->string '(+ a (- b))) "a + -b"))

(define-test "expr->string-unary-minus-higher-precedence"
  (assert-equal (prefix-expr->string '(* a (- b))) "a * -b"))
  
(define-test "expr->string-double-unary-minus"
  (assert-equal (prefix-expr->string '(- (- x))) "-(-x)")) ; or "--x" if unary minus op char is just "-"

(define-test "expr->string-function-simple"
  (assert-equal (prefix-expr->string '(f x y)) "f(x, y)"))

(define-test "expr->string-function-no-args"
  (assert-equal (prefix-expr->string '(g)) "g()"))

(define-test "expr->string-function-nested"
  (assert-equal (prefix-expr->string '(f (g x) y)) "f(g(x), y)"))

(define-test "expr->string-function-with-expr-args"
  (assert-equal (prefix-expr->string '(f (+ x 1) (* y z))) "f(x + 1, y * z)"))

(define-test "expr->string-variadic-plus"
  (assert-equal (prefix-expr->string '(+ a b c d)) "a + b + c + d"))

(define-test "expr->string-variadic-times"
  (assert-equal (prefix-expr->string '(* a b c d)) "a * b * c * d"))

(define-test "expr->string-variadic-plus-times-mixed"
  (assert-equal (prefix-expr->string '(+ a (* b c) d (* e f g))) "a + b * c + d + e * f * g"))

(define-test "expr->string-complex-from-expand"
  ;; (+ (* 3 a (^ x 2)) (* 3 x (^ a 2)) (^ a 3) (^ x 3))
  ;; Expected: "3 * a * x ^ 2 + 3 * x * a ^ 2 + a ^ 3 + x ^ 3" (order might vary based on term<? if simplify was involved)
  ;; For direct conversion without prior sort:
  (assert-equal (prefix-expr->string '(+ (* 3 a (^ x 2)) (* 3 x (^ a 2)) (^ a 3) (^ x 3)))
                "3 * a * x ^ 2 + 3 * x * a ^ 2 + a ^ 3 + x ^ 3"))

(define-test "expr->string-division"
  (assert-equal (prefix-expr->string '(/ (+ a b) c)) "(a + b) / c"))

(define-test "expr->string-unary-plus-identity" ; Assuming (+ x) prints as x
  (assert-equal (prefix-expr->string '(+ x)) "x"))

(define-test "expr->string-unary-times-identity" ; Assuming (* x) prints as x
  (assert-equal (prefix-expr->string '(* x)) "x"))

(define-test "expr->string-unary-plus-in-expr" ; Assuming (+ x) prints as x
  (assert-equal (prefix-expr->string '(+ (+ x) y)) "x + y"))


;;;-----------------------------------------------------------------------------
;;; Tests for prefix-expr->markdown-latex (Expression to LaTeX String)
;;;-----------------------------------------------------------------------------

(define-test "expr->latex-simple-add"
  (assert-equal (prefix-expr->markdown-latex '(+ 1 2)) "$1 + 2$"))

(define-test "expr->latex-simple-vars"
  (assert-equal (prefix-expr->markdown-latex '(* a b)) "$a \\cdot b$"))

(define-test "expr->latex-precedence-1"
  (assert-equal (prefix-expr->markdown-latex '(+ 1 (* 2 3))) "$1 + 2 \\cdot 3$"))

(define-test "expr->latex-precedence-2"
  (assert-equal (prefix-expr->markdown-latex '(* (+ 1 2) 3)) "$\\left(1 + 2\\right) \\cdot 3$"))

(define-test "expr->latex-division"
  (assert-equal (prefix-expr->markdown-latex '(/ x 2)) "$\\frac{x}{2}$"))

(define-test "expr->latex-division-complex-num"
  (assert-equal (prefix-expr->markdown-latex '(/ (+ x y) 2)) "$\\frac{x + y}{2}$"))

(define-test "expr->latex-division-complex-den"
  (assert-equal (prefix-expr->markdown-latex '(/ x (+ y 2))) "$\\frac{x}{y + 2}$"))

(define-test "expr->latex-power"
  (assert-equal (prefix-expr->markdown-latex '(^ x 2)) "$x^{2}$"))

(define-test "expr->latex-power-complex-base"
  (assert-equal (prefix-expr->markdown-latex '(^ (+ x y) 2)) "$\\left(x + y\\right)^{2}$"))

(define-test "expr->latex-power-complex-exp"
  (assert-equal (prefix-expr->markdown-latex '(^ x (+ y 2))) "$x^{y + 2}$"))
  
(define-test "expr->latex-power-right-assoc"
  (assert-equal (prefix-expr->markdown-latex '(^ x (^ y z))) "$x^{y^{z}}$"))

(define-test "expr->latex-unary-minus-simple"
  (assert-equal (prefix-expr->markdown-latex '(- x)) "$-x$"))

(define-test "expr->latex-unary-minus-in-expr"
  (assert-equal (prefix-expr->markdown-latex '(+ a (- b))) "$a + -b$")) ; or "$a - b$" if (+ a (-b)) simplifies first

(define-test "expr->latex-function-sin"
  (assert-equal (prefix-expr->markdown-latex '(sin x)) "$\\sin\\left(x\\right)$"))

(define-test "expr->latex-function-cos-expr"
  (assert-equal (prefix-expr->markdown-latex '(cos (+ x pi))) "$\\cos\\left(x + \\pi\\right)$"))

(define-test "expr->latex-function-sqrt"
  (assert-equal (prefix-expr->markdown-latex '(sqrt (+ (^ x 2) (^ y 2)))) "$\\sqrt{x^{2} + y^{2}}$"))

(define-test "expr->latex-variadic-plus"
  (assert-equal (prefix-expr->markdown-latex '(+ a b c d)) "$a + b + c + d$"))

(define-test "expr->latex-variadic-times"
  (assert-equal (prefix-expr->markdown-latex '(* a b c d)) "$a \\cdot b \\cdot c \\cdot d$"))

(define-test "expr->latex-complex-from-expand"
  ;; (+ (* 3 a (^ x 2)) (* 3 x (^ a 2)) (^ a 3) (^ x 3))
  (assert-equal (prefix-expr->markdown-latex '(+ (* 3 a (^ x 2)) (* 3 x (^ a 2)) (^ a 3) (^ x 3)))
                "$3 \\cdot a \\cdot x^{2} + 3 \\cdot x \\cdot a^{2} + a^{3} + x^{3}$"))

(define-test "expr->latex-sin-squared-x"
  (assert-equal (prefix-expr->markdown-latex '(^ (sin x) 2)) "$\\sin^{2}\\left(x\\right)$"))

(define-test "expr->latex-log-base-exp"
  (assert-equal (prefix-expr->markdown-latex '(log (^ x 2))) "$\\log\\left(x^{2}\\right)$"))

(define-test "expr->latex-unary-plus-identity" ; Assuming (+ x) prints as x
  (assert-equal (prefix-expr->markdown-latex '(+ x)) "$x$"))

(define-test "expr->latex-unary-times-identity" ; Assuming (* x) prints as x
  (assert-equal (prefix-expr->markdown-latex '(* x)) "$x$"))
