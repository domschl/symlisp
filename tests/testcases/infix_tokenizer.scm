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
