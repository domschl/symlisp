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
