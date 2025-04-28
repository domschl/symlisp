;;; Tests for Core Special Forms

;; --- QUOTE ---
(define-test "quote-symbol" (assert-equal (quote abc) 'abc))
(define-test "quote-number" (assert-equal (quote 123) 123))
(define-test "quote-list" (assert-equal (quote (1 2 3)) '(1 2 3)))
(define-test "quote-nested-list" (assert-equal (quote (1 (2 3) 4)) '(1 (2 3) 4)))
(define-test "quote-true" (assert-equal (quote #t) #t))
(define-test "quote-false" (assert-equal (quote #f) #f))
(define-test "quote-nil" (assert-equal (quote ()) '()))

;; --- IF ---
(define-test "if-true-conseq" (assert-equal (if #t 1 2) 1))
(define-test "if-false-alt" (assert-equal (if #f 1 2) 2))
(define-test "if-nil-alt" (assert-equal (if '() 1 2) 2)) ; NIL is falsey
(define-test "if-zero-conseq" (assert-equal (if 0 1 2) 1)) ; 0 is truthy
(define-test "if-empty-list-conseq" (assert-equal (if (list) 1 2) 1)) ; Empty list is truthy
(define-test "if-no-alt-true" (assert-equal (if #t 5) 5))
(define-test "if-no-alt-false" (assert-equal (if #f 5) '())) ; Result unspecified, using NIL

;; --- DEFINE / SET! ---
(define test-var-1 10)
(define-test "define-var" (assert-equal test-var-1 10))
(set! test-var-1 20)
(define-test "set!-var" (assert-equal test-var-1 20))

(define (test-func-1 x) (+ x 1))
(define-test "define-func" (assert-equal (test-func-1 5) 6))

(define test-func-2 (lambda (y) (* y 2)))
(define-test "define-lambda" (assert-equal (test-func-2 7) 14))

;; --- LAMBDA ---
(define-test "lambda-immediate-call" (assert-equal ((lambda (x) (+ x 5)) 3) 8))
(define-test "lambda-no-args" (assert-equal ((lambda () 42)) 42))
(define closure-test-var 100)
(define closure-test-func (lambda (x) (+ x closure-test-var)))
(define-test "lambda-closure-capture" (assert-equal (closure-test-func 5) 105))
(set! closure-test-var 200) ; Change captured variable
(define-test "lambda-closure-capture-after-set" (assert-equal (closure-test-func 5) 205)) ; Should see new value

;; --- BEGIN ---
(define begin-test-var 0)
(define-test "begin-sequence"
  (assert-equal (begin (set! begin-test-var 1) (set! begin-test-var 2) (+ begin-test-var 3)) 5))
(define-test "begin-var-state" (assert-equal begin-test-var 2))
(define-test "begin-single-expr" (assert-equal (begin 99) 99))
(define-test "begin-empty" (assert-equal (begin) '())) ; Result unspecified, using NIL

;; --- LET ---
(define-test "let-simple" (assert-equal (let ((x 1) (y 2)) (+ x y)) 3))
(define-test "let-sequential-init" (assert-equal (let ((x 1) (y (+ x 1))) (+ x y)) 3)) ; Standard let inits use outer scope
(define let-outer-var 10)
(define-test "let-uses-outer-scope" (assert-equal (let ((x let-outer-var)) x) 10))
(define-test "let-shadows-outer" (assert-equal (let ((let-outer-var 5)) let-outer-var) 5))
(define-test "let-outer-unaffected" (assert-equal let-outer-var 10))
(define-test "let-empty-bindings" (assert-equal (let () 42) 42))
(define-test "let-multi-body" (assert-equal (let ((x 1)) (set! x (+ x 1)) x) 2))

;; --- NAMED LET ---
(define-test "named-let-factorial"
  (assert-equal (let fact ((n 5)) (if (= n 0) 1 (* n (fact (- n 1))))) 120))

(define-test "named-let-countdown"
  (assert-equal (let loop ((i 3) (acc '()))
                  (if (< i 0)
                      acc
                      (loop (- i 1) (cons i acc))))
                '(0 1 2 3))) ; Note: cons builds list in reverse order of iteration

(define-test "named-let-access-outer"
  (let ((outer 10))
    (assert-equal (let inner ((x 1)) (+ x outer)) 11)))