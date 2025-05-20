;; --- Type Predicates ---

;; boolean?
(define-test-thunked "boolean?-true" (lambda () (assert-equal (boolean? #t) #t)))
(define-test-thunked "boolean?-false" (lambda () (assert-equal (boolean? #f) #t)))
(define-test-thunked "boolean?-nil" (lambda () (assert-equal (boolean? '()) #f)))
(define-test-thunked "boolean?-number" (lambda () (assert-equal (boolean? 0) #f)))
(define-test-thunked "boolean?-string" (lambda () (assert-equal (boolean? "hello") #f)))
(define-test-thunked "boolean?-symbol" (lambda () (assert-equal (boolean? 'hello) #f)))
(define-test-thunked "boolean?-pair" (lambda () (assert-equal (boolean? '(1 . 2)) #f)))
(define-test-thunked "boolean?-procedure" (lambda () (assert-equal (boolean? +) #f)))

;; pair?
(define-test-thunked "pair?-pair" (lambda () (assert-equal (pair? '(1 . 2)) #t)))
(define-test-thunked "pair?-list" (lambda () (assert-equal (pair? '(1 2 3)) #t))) ; Lists start with pairs
(define-test-thunked "pair?-nil" (lambda () (assert-equal (pair? '()) #f)))
(define-test-thunked "pair?-symbol" (lambda () (assert-equal (pair? 'a) #f)))
(define-test-thunked "pair?-number" (lambda () (assert-equal (pair? 1) #f)))
(define-test-thunked "pair?-string" (lambda () (assert-equal (pair? "a") #f)))
(define-test-thunked "pair?-boolean" (lambda () (assert-equal (pair? #t) #f)))
(define-test-thunked "pair?-procedure" (lambda () (assert-equal (pair? car) #f)))

;; symbol?
(define-test-thunked "symbol?-symbol" (lambda () (assert-equal (symbol? 'hello) #t)))
(define-test-thunked "symbol?-quoted-list" (lambda () (assert-equal (symbol? ''hello) #f)))
(define-test-thunked "symbol?-nil" (lambda () (assert-equal (symbol? '()) #f)))
(define-test-thunked "symbol?-number" (lambda () (assert-equal (symbol? 123) #f)))
(define-test-thunked "symbol?-string" (lambda () (assert-equal (symbol? "hello") #f)))
(define-test-thunked "symbol?-boolean" (lambda () (assert-equal (symbol? #f) #f)))
(define-test-thunked "symbol?-pair" (lambda () (assert-equal (symbol? '(a b)) #f)))
(define-test-thunked "symbol?-procedure" (lambda () (assert-equal (symbol? +) #f)))

;; number?
(define-test-thunked "number?-integer" (lambda () (assert-equal (number? 123) #t)))
(define-test-thunked "number?-negative-integer" (lambda () (assert-equal (number? -45) #t)))
(define-test-thunked "number?-zero" (lambda () (assert-equal (number? 0) #t)))
(define-test-thunked "number?-fraction" (lambda () (assert-equal (number? 3/4) #t)))
(define-test-thunked "number?-negative-fraction" (lambda () (assert-equal (number? -1/2) #t)))
(define-test-thunked "number?-symbol" (lambda () (assert-equal (number? 'a) #f)))
(define-test-thunked "number?-string" (lambda () (assert-equal (number? "123") #f)))
(define-test-thunked "number?-boolean" (lambda () (assert-equal (number? #t) #f)))
(define-test-thunked "number?-nil" (lambda () (assert-equal (number? '()) #f)))
(define-test-thunked "number?-pair" (lambda () (assert-equal (number? '(1)) #f)))
(define-test-thunked "number?-procedure" (lambda () (assert-equal (number? +) #f)))

;; string?
(define-test-thunked "string?-string" (lambda () (assert-equal (string? "hello") #t)))
(define-test-thunked "string?-empty-string" (lambda () (assert-equal (string? "") #t)))
(define-test-thunked "string?-symbol" (lambda () (assert-equal (string? 'hello) #f)))
(define-test-thunked "string?-number" (lambda () (assert-equal (string? 123) #f)))
(define-test-thunked "string?-boolean" (lambda () (assert-equal (string? #f) #f)))
(define-test-thunked "string?-nil" (lambda () (assert-equal (string? '()) #f)))
(define-test-thunked "string?-pair" (lambda () (assert-equal (string? '("a")) #f)))
(define-test-thunked "string?-procedure" (lambda () (assert-equal (string? display) #f)))

;; procedure?
(define-test-thunked "procedure?-builtin" (lambda () (assert-equal (procedure? +) #t)))
(define-test-thunked "procedure?-lambda" (lambda () (assert-equal (procedure? (lambda (x) x)) #t)))
(define-test-thunked "procedure?-symbol" (lambda () (assert-equal (procedure? 'car) #f)))
(define-test-thunked "procedure?-number" (lambda () (assert-equal (procedure? 1) #f)))
(define-test-thunked "procedure?-string" (lambda () (assert-equal (procedure? "lambda") #f)))
(define-test-thunked "procedure?-boolean" (lambda () (assert-equal (procedure? #t) #f)))
(define-test-thunked "procedure?-nil" (lambda () (assert-equal (procedure? '()) #f)))
(define-test-thunked "procedure?-pair" (lambda () (assert-equal (procedure? '(lambda (x) x)) #f))) ; The list itself is not a procedure

;; null?
(define-test-thunked "null?-nil" (lambda () (assert-equal (null? '()) #t)))
(define-test-thunked "null?-empty-list-constructor" (lambda () (assert-equal (null? (list)) #t)))
(define-test-thunked "null?-false" (lambda () (assert-equal (null? #f) #f)))
(define-test-thunked "null?-zero" (lambda () (assert-equal (null? 0) #f)))
(define-test-thunked "null?-empty-string" (lambda () (assert-equal (null? "") #f)))
(define-test-thunked "null?-non-empty-list" (lambda () (assert-equal (null? '(1)) #f)))
(define-test-thunked "null?-pair" (lambda () (assert-equal (null? '(1 . 2)) #f)))

;; list?
(define-test-thunked "list?-proper-list" (lambda () (assert-equal (list? '(1 2 3)) #t)))
(define-test-thunked "list?-empty-list" (lambda () (assert-equal (list? '()) #t)))
(define-test-thunked "list?-list-constructor" (lambda () (assert-equal (list? (list 1 2)) #t)))
(define-test-thunked "list?-improper-list" (lambda () (assert-equal (list? '(1 2 . 3)) #f)))
(define-test-thunked "list?-dotted-pair" (lambda () (assert-equal (list? '(1 . 2)) #f)))
(define-test-thunked "list?-symbol" (lambda () (assert-equal (list? 'a) #f)))
(define-test-thunked "list?-number" (lambda () (assert-equal (list? 1) #f)))
(define-test-thunked "list?-string" (lambda () (assert-equal (list? "abc") #f)))
(define-test-thunked "list?-boolean" (lambda () (assert-equal (list? #t) #f)))
(define-test-thunked "list?-procedure" (lambda () (assert-equal (list? +) #f)))
; (define-test-thunked "list?-circular" (lambda () (let ((x (list 'a))) (set-cdr! x x) (assert-equal (list? x) #f)))) ; Requires set-cdr!

;;;-----------------------------------------------------------------------------
;;; Character Predicates
;;;-----------------------------------------------------------------------------

;;; char-alphabetic? Tests
(define-test-thunked "char-alphabetic?-lower-ascii" (lambda () (assert-true (char-alphabetic? #\a))))
(define-test-thunked "char-alphabetic?-upper-ascii" (lambda () (assert-true (char-alphabetic? #\Z))))
(define-test-thunked "char-alphabetic?-digit" (lambda () (assert-false (char-alphabetic? #\5))))
(define-test-thunked "char-alphabetic?-space" (lambda () (assert-false (char-alphabetic? #\space))))
(define-test-thunked "char-alphabetic?-punct" (lambda () (assert-false (char-alphabetic? #\.))))
(define-test-thunked "char-alphabetic?-latin1" (lambda () (assert-true (char-alphabetic? #\é)))) ; U+00E9
(define-test-thunked "char-alphabetic?-greek" (lambda () (assert-true (char-alphabetic? #\Ω)))) ; U+03A9
(define-test-thunked "char-alphabetic?-cyrillic" (lambda () (assert-true (char-alphabetic? #\я)))) ; U+044F
(define-test-thunked "char-alphabetic?-armenian" (lambda () (assert-true (char-alphabetic? #\Ա)))) ; U+0531
(define-test-thunked "char-alphabetic?-georgian" (lambda () (assert-true (char-alphabetic? #\ა)))) ; U+10D0
;(define-test-thunked "char-alphabetic?-error-type" (lambda () (assert-error? (char-alphabetic? "a"))))
;(define-test-thunked "char-alphabetic?-error-arity" (lambda () (assert-error? (char-alphabetic? #\a #\b))))

;;; char-numeric? Tests
(define-test-thunked "char-numeric?-digit" (lambda () (assert-true (char-numeric? #\7))))
(define-test-thunked "char-numeric?-zero" (lambda () (assert-true (char-numeric? #\0))))
(define-test-thunked "char-numeric?-alpha" (lambda () (assert-false (char-numeric? #\a))))
(define-test-thunked "char-numeric?-space" (lambda () (assert-false (char-numeric? #\space))))
(define-test-thunked "char-numeric?-punct" (lambda () (assert-false (char-numeric? #\.))))
; Note: This only tests ASCII 0-9 based on current C implementation
;(define-test-thunked "char-numeric?-error-type" (lambda () (assert-error? (char-numeric? 7))))
;(define-test-thunked "char-numeric?-error-arity" (lambda () (assert-error? (char-numeric? #\1 #\2))))

;;; char-whitespace? Tests
(define-test-thunked "char-whitespace?-space" (lambda () (assert-true (char-whitespace? #\space))))
(define-test-thunked "char-whitespace?-tab" (lambda () (assert-true (char-whitespace? #\tab))))
(define-test-thunked "char-whitespace?-newline" (lambda () (assert-true (char-whitespace? #\newline))))
(define-test-thunked "char-whitespace?-return" (lambda () (assert-true (char-whitespace? #\return)))) ; Assuming #\return maps to \r
(define-test-thunked "char-whitespace?-formfeed" (lambda () (assert-true (char-whitespace? #\page)))) ; Assuming #\page maps to \f
(define-test-thunked "char-whitespace?-nbsp" (lambda () (assert-true (char-whitespace? #\x00A0)))) ; Non-breaking space
(define-test-thunked "char-whitespace?-alpha" (lambda () (assert-false (char-whitespace? #\a))))
(define-test-thunked "char-whitespace?-digit" (lambda () (assert-false (char-whitespace? #\0))))
(define-test-thunked "char-whitespace?-punct" (lambda () (assert-false (char-whitespace? #\.))))
;(define-test-thunked "char-whitespace?-error-type" (lambda () (assert-error? (char-whitespace? " "))))
;(define-test-thunked "char-whitespace?-error-arity" (lambda () (assert-error? (char-whitespace? #\space #\tab))))

;;; char-upper-case? Tests
(define-test-thunked "char-upper-case?-upper-ascii" (lambda () (assert-true (char-upper-case? #\A))))
(define-test-thunked "char-upper-case?-lower-ascii" (lambda () (assert-false (char-upper-case? #\a))))
(define-test-thunked "char-upper-case?-digit" (lambda () (assert-false (char-upper-case? #\5))))
(define-test-thunked "char-upper-case?-upper-latin1" (lambda () (assert-true (char-upper-case? #\Ö)))) ; U+00D6
(define-test-thunked "char-upper-case?-lower-latin1" (lambda () (assert-false (char-upper-case? #\ö)))) ; U+00F6
(define-test-thunked "char-upper-case?-upper-greek" (lambda () (assert-true (char-upper-case? #\Ω)))) ; U+03A9
(define-test-thunked "char-upper-case?-lower-greek" (lambda () (assert-false (char-upper-case? #\ω)))) ; U+03C9
(define-test-thunked "char-upper-case?-greek-tonos" (lambda () (assert-true (char-upper-case? #\Ά)))) ; U+0386
(define-test-thunked "char-upper-case?-cyrillic" (lambda () (assert-true (char-upper-case? #\Я)))) ; U+042F
(define-test-thunked "char-upper-case?-armenian" (lambda () (assert-true (char-upper-case? #\Ֆ)))) ; U+0556
(define-test-thunked "char-upper-case?-georgian" (lambda () (assert-true (char-upper-case? #\Ჰ)))) ; U+1CB0
;(define-test-thunked "char-upper-case?-error-type" (lambda () (assert-error? (char-upper-case? "A"))))
;(define-test-thunked "char-upper-case?-error-arity" (lambda () (assert-error? (char-upper-case? #\A #\B))))

;;; char-lower-case? Tests
(define-test-thunked "char-lower-case?-lower-ascii" (lambda () (assert-true (char-lower-case? #\a))))
(define-test-thunked "char-lower-case?-upper-ascii" (lambda () (assert-false (char-lower-case? #\A))))
(define-test-thunked "char-lower-case?-digit" (lambda () (assert-false (char-lower-case? #\5))))
(define-test-thunked "char-lower-case?-lower-latin1" (lambda () (assert-true (char-lower-case? #\ö)))) ; U+00F6
(define-test-thunked "char-lower-case?-upper-latin1" (lambda () (assert-false (char-lower-case? #\Ö)))) ; U+00D6
(define-test-thunked "char-lower-case?-lower-greek" (lambda () (assert-true (char-lower-case? #\ω)))) ; U+03C9
(define-test-thunked "char-lower-case?-upper-greek" (lambda () (assert-false (char-lower-case? #\Ω)))) ; U+03A9
(define-test-thunked "char-lower-case?-greek-tonos" (lambda () (assert-true (char-lower-case? #\ά)))) ; U+03AC
(define-test-thunked "char-lower-case?-cyrillic" (lambda () (assert-true (char-lower-case? #\я)))) ; U+044F
(define-test-thunked "char-lower-case?-armenian" (lambda () (assert-true (char-lower-case? #\ֆ)))) ; U+0586
(define-test-thunked "char-lower-case?-georgian" (lambda () (assert-true (char-lower-case? #\ჰ)))) ; U+10F0
;(define-test-thunked "char-lower-case?-error-type" (lambda () (assert-error? (char-lower-case? "a"))))
;(define-test-thunked "char-lower-case?-error-arity" (lambda () (assert-error? (char-lower-case? #\a #\b))))

;;;-----------------------------------------------------------------------------
;;; Numeric Predicates (Additional)
;;;-----------------------------------------------------------------------------

;;; integer? Tests
(define-test-thunked "integer?-small-int" (lambda () (assert-true (integer? 5))))
(define-test-thunked "integer?-zero" (lambda () (assert-true (integer? 0))))
(define-test-thunked "integer?-negative-small-int" (lambda () (assert-true (integer? -10))))
(define-test-thunked "integer?-big-int" (lambda () (assert-true (integer? 12345678901234567890)))) ; Assumes this triggers bignum
(define-test-thunked "integer?-negative-big-int" (lambda () (assert-true (integer? -12345678901234567890))))
(define-test-thunked "integer?-rational-int" (lambda () (assert-true (integer? (/ 6 2))))); Should simplify to 3
(define-test-thunked "integer?-rational-not-int" (lambda () (assert-false (integer? (/ 1 2)))))
(define-test-thunked "integer?-rational-big-int" (lambda () (assert-true (integer? (/ 24691357802469135780 2)))))
(define-test-thunked "integer?-rational-big-not-int" (lambda () (assert-true (integer? (/ 12345678901234567890 3)))))
(define-test-thunked "integer?-rational-big-not-int" (lambda () (assert-false (integer? (/ 12345678901234567891 3)))))
(define-test-thunked "integer?-string" (lambda () (assert-false (integer? "5"))))
(define-test-thunked "integer?-symbol" (lambda () (assert-false (integer? 'a))))
(define-test-thunked "integer?-list" (lambda () (assert-false (integer? '(1)))))
;(define-test-thunked "integer?-error-arity" (lambda () (assert-error? (integer? 1 2))))

;; odd/even? Tests
(define-test-thunked "odd?-odd" (lambda () (assert-true (odd? 3))))
(define-test-thunked "odd?-even" (lambda () (assert-false (odd? 4))))
(define-test-thunked "odd?-zero" (lambda () (assert-false (odd? 0))))
(define-test-thunked "odd?-negative-odd" (lambda () (assert-true (odd? -5))))
(define-test-thunked "odd?-negative-even" (lambda () (assert-false (odd? -6))))
(define-test-thunked "odd?-big-odd" (lambda () (assert-true (odd? 123456789))))
(define-test-thunked "odd?-big-even" (lambda () (assert-false (odd? 123456788))))

;;; prime? Tests
(define-test-thunked "prime?-2" (lambda () (assert-true (prime? 2))))
(define-test-thunked "prime?-3" (lambda () (assert-true (prime? 3))))
(define-test-thunked "prime?-4" (lambda () (assert-false (prime? 4))))
(define-test-thunked "prime?-5" (lambda () (assert-true (prime? 5))))
(define-test-thunked "prime?-7" (lambda () (assert-true (prime? 7))))
(define-test-thunked "prime?-9" (lambda () (assert-false (prime? 9))))
(define-test-thunked "prime?-10" (lambda () (assert-false (prime? 10))))
(define-test-thunked "prime?-11" (lambda () (assert-true (prime? 11))))
(define-test-thunked "prime?-large-prime" (lambda () (assert-true (prime? 7919)))) ; A known prime
(define-test-thunked "prime?-large-composite" (lambda () (assert-false (prime? 7921)))) ; 89*89
(define-test-thunked "prime?-big-prime" (lambda () (assert-true (prime? 2147483647)))) ; Mersenne prime 2^31-1
(define-test-thunked "prime?-big-composite" (lambda () (assert-false (prime? 2147483649)))) ; Fermat F5 = 641 * 6700417
(define-test-thunked "prime?-1" (lambda () (assert-false (prime? 1))))
(define-test-thunked "prime?-0" (lambda () (assert-false (prime? 0))))
(define-test-thunked "prime?-negative" (lambda () (assert-false (prime? -7))))
(define-test-thunked "prime?-rational-prime-num" (lambda () (assert-false (prime? (/ 7 2))))) ; Not an integer
(define-test-thunked "prime?-rational-int-prime" (lambda () (assert-true (prime? (/ 14 2))))) ; Is integer 7
(define-test-thunked "prime?-rational-int-composite" (lambda () (assert-false (prime? (/ 18 2))))) ; Is integer 9
(define-test-thunked "prime?-string" (lambda () (assert-false (prime? "7"))))
(define-test-thunked "prime?-symbol" (lambda () (assert-false (prime? 'prime))))
;(define-test-thunked "prime?-error-arity" (lambda () (assert-error? (prime? 7 11))))

;;; --- END OF PREDICATE TESTS ---