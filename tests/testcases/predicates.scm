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

;;; --- END OF PREDICATE TESTS ---