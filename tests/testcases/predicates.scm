;; --- Type Predicates ---

;; boolean?
(define-test "boolean?-true" (assert-equal (boolean? #t) #t))
(define-test "boolean?-false" (assert-equal (boolean? #f) #t))
(define-test "boolean?-nil" (assert-equal (boolean? '()) #f))
(define-test "boolean?-number" (assert-equal (boolean? 0) #f))
(define-test "boolean?-string" (assert-equal (boolean? "hello") #f))
(define-test "boolean?-symbol" (assert-equal (boolean? 'hello) #f))
(define-test "boolean?-pair" (assert-equal (boolean? '(1 . 2)) #f))
(define-test "boolean?-procedure" (assert-equal (boolean? +) #f))

;; pair?
(define-test "pair?-pair" (assert-equal (pair? '(1 . 2)) #t))
(define-test "pair?-list" (assert-equal (pair? '(1 2 3)) #t)) ; Lists start with pairs
(define-test "pair?-nil" (assert-equal (pair? '()) #f))
(define-test "pair?-symbol" (assert-equal (pair? 'a) #f))
(define-test "pair?-number" (assert-equal (pair? 1) #f))
(define-test "pair?-string" (assert-equal (pair? "a") #f))
(define-test "pair?-boolean" (assert-equal (pair? #t) #f))
(define-test "pair?-procedure" (assert-equal (pair? car) #f))

;; symbol?
(define-test "symbol?-symbol" (assert-equal (symbol? 'hello) #t))
(define-test "symbol?-quoted-list" (assert-equal (symbol? ''hello) #f))
(define-test "symbol?-nil" (assert-equal (symbol? '()) #f))
(define-test "symbol?-number" (assert-equal (symbol? 123) #f))
(define-test "symbol?-string" (assert-equal (symbol? "hello") #f))
(define-test "symbol?-boolean" (assert-equal (symbol? #f) #f))
(define-test "symbol?-pair" (assert-equal (symbol? '(a b)) #f))
(define-test "symbol?-procedure" (assert-equal (symbol? +) #f))

;; number?
(define-test "number?-integer" (assert-equal (number? 123) #t))
(define-test "number?-negative-integer" (assert-equal (number? -45) #t))
(define-test "number?-zero" (assert-equal (number? 0) #t))
(define-test "number?-fraction" (assert-equal (number? 3/4) #t))
(define-test "number?-negative-fraction" (assert-equal (number? -1/2) #t))
(define-test "number?-symbol" (assert-equal (number? 'a) #f))
(define-test "number?-string" (assert-equal (number? "123") #f))
(define-test "number?-boolean" (assert-equal (number? #t) #f))
(define-test "number?-nil" (assert-equal (number? '()) #f))
(define-test "number?-pair" (assert-equal (number? '(1)) #f))
(define-test "number?-procedure" (assert-equal (number? +) #f))

;; string?
(define-test "string?-string" (assert-equal (string? "hello") #t))
(define-test "string?-empty-string" (assert-equal (string? "") #t))
(define-test "string?-symbol" (assert-equal (string? 'hello) #f))
(define-test "string?-number" (assert-equal (string? 123) #f))
(define-test "string?-boolean" (assert-equal (string? #f) #f))
(define-test "string?-nil" (assert-equal (string? '()) #f))
(define-test "string?-pair" (assert-equal (string? '("a")) #f))
(define-test "string?-procedure" (assert-equal (string? display) #f))

;; procedure?
(define-test "procedure?-builtin" (assert-equal (procedure? +) #t))
(define-test "procedure?-lambda" (assert-equal (procedure? (lambda (x) x)) #t))
(define-test "procedure?-symbol" (assert-equal (procedure? 'car) #f))
(define-test "procedure?-number" (assert-equal (procedure? 1) #f))
(define-test "procedure?-string" (assert-equal (procedure? "lambda") #f))
(define-test "procedure?-boolean" (assert-equal (procedure? #t) #f))
(define-test "procedure?-nil" (assert-equal (procedure? '()) #f))
(define-test "procedure?-pair" (assert-equal (procedure? '(lambda (x) x)) #f)) ; The list itself is not a procedure

;; null?
(define-test "null?-nil" (assert-equal (null? '()) #t))
(define-test "null?-empty-list-constructor" (assert-equal (null? (list)) #t))
(define-test "null?-false" (assert-equal (null? #f) #f))
(define-test "null?-zero" (assert-equal (null? 0) #f))
(define-test "null?-empty-string" (assert-equal (null? "") #f))
(define-test "null?-non-empty-list" (assert-equal (null? '(1)) #f))
(define-test "null?-pair" (assert-equal (null? '(1 . 2)) #f))

;; list?
(define-test "list?-proper-list" (assert-equal (list? '(1 2 3)) #t))
(define-test "list?-empty-list" (assert-equal (list? '()) #t))
(define-test "list?-list-constructor" (assert-equal (list? (list 1 2)) #t))
(define-test "list?-improper-list" (assert-equal (list? '(1 2 . 3)) #f))
(define-test "list?-dotted-pair" (assert-equal (list? '(1 . 2)) #f))
(define-test "list?-symbol" (assert-equal (list? 'a) #f))
(define-test "list?-number" (assert-equal (list? 1) #f))
(define-test "list?-string" (assert-equal (list? "abc") #f))
(define-test "list?-boolean" (assert-equal (list? #t) #f))
(define-test "list?-procedure" (assert-equal (list? +) #f))
; (define-test "list?-circular" (let ((x (list 'a))) (set-cdr! x x) (assert-equal (list? x) #f))) ; Requires set-cdr!

;;;-----------------------------------------------------------------------------
;;; Character Predicates
;;;-----------------------------------------------------------------------------

;;; char-alphabetic? Tests
(define-test "char-alphabetic?-lower-ascii" (assert-true (char-alphabetic? #\a)))
(define-test "char-alphabetic?-upper-ascii" (assert-true (char-alphabetic? #\Z)))
(define-test "char-alphabetic?-digit" (assert-false (char-alphabetic? #\5)))
(define-test "char-alphabetic?-space" (assert-false (char-alphabetic? #\space)))
(define-test "char-alphabetic?-punct" (assert-false (char-alphabetic? #\.)))
(define-test "char-alphabetic?-latin1" (assert-true (char-alphabetic? #\é))) ; U+00E9
(define-test "char-alphabetic?-greek" (assert-true (char-alphabetic? #\Ω))) ; U+03A9
(define-test "char-alphabetic?-cyrillic" (assert-true (char-alphabetic? #\я))) ; U+044F
(define-test "char-alphabetic?-armenian" (assert-true (char-alphabetic? #\Ա))) ; U+0531
(define-test "char-alphabetic?-georgian" (assert-true (char-alphabetic? #\ა))) ; U+10D0
;(define-test "char-alphabetic?-error-type" (assert-error? (char-alphabetic? "a")))
;(define-test "char-alphabetic?-error-arity" (assert-error? (char-alphabetic? #\a #\b)))

;;; char-numeric? Tests
(define-test "char-numeric?-digit" (assert-true (char-numeric? #\7)))
(define-test "char-numeric?-zero" (assert-true (char-numeric? #\0)))
(define-test "char-numeric?-alpha" (assert-false (char-numeric? #\a)))
(define-test "char-numeric?-space" (assert-false (char-numeric? #\space)))
(define-test "char-numeric?-punct" (assert-false (char-numeric? #\.)))
; Note: This only tests ASCII 0-9 based on current C implementation
;(define-test "char-numeric?-error-type" (assert-error? (char-numeric? 7)))
;(define-test "char-numeric?-error-arity" (assert-error? (char-numeric? #\1 #\2)))

;;; char-whitespace? Tests
(define-test "char-whitespace?-space" (assert-true (char-whitespace? #\space)))
(define-test "char-whitespace?-tab" (assert-true (char-whitespace? #\tab)))
(define-test "char-whitespace?-newline" (assert-true (char-whitespace? #\newline)))
;(define-test "char-whitespace?-return" (assert-true (char-whitespace? #\return))) ; Assuming #\return maps to \r
;(define-test "char-whitespace?-formfeed" (assert-true (char-whitespace? #\page))) ; Assuming #\page maps to \f
;(define-test "char-whitespace?-nbsp" (assert-true (char-whitespace? #\x00A0))) ; Non-breaking space
(define-test "char-whitespace?-alpha" (assert-false (char-whitespace? #\a)))
(define-test "char-whitespace?-digit" (assert-false (char-whitespace? #\0)))
(define-test "char-whitespace?-punct" (assert-false (char-whitespace? #\.)))
;(define-test "char-whitespace?-error-type" (assert-error? (char-whitespace? " ")))
;(define-test "char-whitespace?-error-arity" (assert-error? (char-whitespace? #\space #\tab)))

;;; char-upper-case? Tests
(define-test "char-upper-case?-upper-ascii" (assert-true (char-upper-case? #\A)))
(define-test "char-upper-case?-lower-ascii" (assert-false (char-upper-case? #\a)))
(define-test "char-upper-case?-digit" (assert-false (char-upper-case? #\5)))
(define-test "char-upper-case?-upper-latin1" (assert-true (char-upper-case? #\Ö))) ; U+00D6
(define-test "char-upper-case?-lower-latin1" (assert-false (char-upper-case? #\ö))) ; U+00F6
(define-test "char-upper-case?-upper-greek" (assert-true (char-upper-case? #\Ω))) ; U+03A9
(define-test "char-upper-case?-lower-greek" (assert-false (char-upper-case? #\ω))) ; U+03C9
(define-test "char-upper-case?-greek-tonos" (assert-true (char-upper-case? #\Ά))) ; U+0386
(define-test "char-upper-case?-cyrillic" (assert-true (char-upper-case? #\Я))) ; U+042F
(define-test "char-upper-case?-armenian" (assert-true (char-upper-case? #\Ֆ))) ; U+0556
(define-test "char-upper-case?-georgian" (assert-true (char-upper-case? #\Ჰ))) ; U+1CB0
;(define-test "char-upper-case?-error-type" (assert-error? (char-upper-case? "A")))
;(define-test "char-upper-case?-error-arity" (assert-error? (char-upper-case? #\A #\B)))

;;; char-lower-case? Tests
(define-test "char-lower-case?-lower-ascii" (assert-true (char-lower-case? #\a)))
(define-test "char-lower-case?-upper-ascii" (assert-false (char-lower-case? #\A)))
(define-test "char-lower-case?-digit" (assert-false (char-lower-case? #\5)))
(define-test "char-lower-case?-lower-latin1" (assert-true (char-lower-case? #\ö))) ; U+00F6
(define-test "char-lower-case?-upper-latin1" (assert-false (char-lower-case? #\Ö))) ; U+00D6
(define-test "char-lower-case?-lower-greek" (assert-true (char-lower-case? #\ω))) ; U+03C9
(define-test "char-lower-case?-upper-greek" (assert-false (char-lower-case? #\Ω))) ; U+03A9
(define-test "char-lower-case?-greek-tonos" (assert-true (char-lower-case? #\ά))) ; U+03AC
(define-test "char-lower-case?-cyrillic" (assert-true (char-lower-case? #\я))) ; U+044F
(define-test "char-lower-case?-armenian" (assert-true (char-lower-case? #\ֆ))) ; U+0586
(define-test "char-lower-case?-georgian" (assert-true (char-lower-case? #\ჰ))) ; U+10F0
;(define-test "char-lower-case?-error-type" (assert-error? (char-lower-case? "a")))
;(define-test "char-lower-case?-error-arity" (assert-error? (char-lower-case? #\a #\b)))

;;; --- END OF PREDICATE TESTS ---