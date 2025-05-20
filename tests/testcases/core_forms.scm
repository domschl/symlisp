;;; Tests for Core Special Forms

;; --- QUOTE ---
(define-test "quote-symbol" (lambda () (assert-equal (quote abc) 'abc)))
(define-test "quote-number" (lambda () (assert-equal (quote 123) 123)))
(define-test "quote-list" (lambda () (assert-equal (quote (1 2 3)) '(1 2 3))))
(define-test "quote-nested-list" (lambda () (assert-equal (quote (1 (2 3) 4)) '(1 (2 3) 4))))
(define-test "quote-true" (lambda () (assert-equal (quote #t) #t)))
(define-test "quote-false" (lambda () (assert-equal (quote #f) #f)))
(define-test "quote-nil" (lambda () (assert-equal (quote ()) '())))

;; --- IF ---
(define-test "if-true-conseq" (lambda () (assert-equal (if #t 1 2) 1)))
(define-test "if-false-alt" (lambda () (assert-equal (if #f 1 2) 2)))
(define-test "if-nil-alt" (lambda () (assert-equal (if '() 1 2) 1))) ; NIL is truthy
(define-test "if-zero-conseq" (lambda () (assert-equal (if 0 1 2) 1))) ; 0 is truthy
(define-test "if-empty-list-conseq" (lambda () (assert-equal (if (list) 1 2) 1))) ; Empty list is truthy
(define-test "if-no-alt-true" (lambda () (assert-equal (if #t 5) 5)))
(define-test "if-no-alt-false" (lambda () (assert-equal (if #f 5) '()))) ; Result unspecified, using NIL

;; --- DEFINE / SET! ---
(define test-var-1 10)
(define-test "define-var" (lambda () (assert-equal test-var-1 10)))
(set! test-var-1 20)
(define-test "set!-var" (lambda () (assert-equal test-var-1 20)))

(define (test-func-1 x) (+ x 1))
(define-test "define-func" (lambda () (assert-equal (test-func-1 5) 6)))

(define test-func-2 (lambda (y) (* y 2)))
(define-test "define-lambda" (lambda () (assert-equal (test-func-2 7) 14)))

;; --- LAMBDA ---
(define-test "lambda-immediate-call" (lambda () (assert-equal ((lambda (x) (+ x 5)) 3) 8)))
(define-test "lambda-no-args" (lambda () (assert-equal ((lambda () 42)) 42)))
(define closure-test-var 100)
(define closure-test-func (lambda (x) (+ x closure-test-var)))
(define-test "lambda-closure-capture" (lambda () (assert-equal (closure-test-func 5) 105)))
(set! closure-test-var 200) ; Change captured variable
(define-test "lambda-closure-capture-after-set" (lambda () (assert-equal (closure-test-func 5) 205))) ; Should see new value

;; --- BEGIN ---
(define begin-test-var 0)
(define-test "begin-sequence"
  (lambda () (assert-equal (begin (set! begin-test-var 1) (set! begin-test-var 2) (+ begin-test-var 3)) 5)))
(define-test "begin-var-state" (lambda () (assert-equal begin-test-var 2)))
(define-test "begin-single-expr" (lambda () (assert-equal (begin 99) 99)))
(define-test "begin-empty" (lambda () (assert-equal (begin) '()))) ; Result unspecified, using NIL

;; --- LET ---
(define-test "let-simple" (lambda () (assert-equal (let ((x 1) (y 2)) (+ x y)) 3)))
(define let-outer-var 10)
(define-test "let-uses-outer-scope" (lambda () (assert-equal (let ((x let-outer-var)) x) 10)))
(define-test "let-shadows-outer" (lambda () (assert-equal (let ((let-outer-var 5)) let-outer-var) 5)))
(define-test "let-outer-unaffected" (lambda () (assert-equal let-outer-var 10)))
(define-test "let-empty-bindings" (lambda () (assert-equal (let () 42) 42)))
(define-test "let-multi-body" (lambda () (assert-equal (let ((x 1)) (set! x (+ x 1)) x) 2)))

;; --- NAMED LET ---
(define-test "named-let-factorial"
  (lambda () (assert-equal (let fact ((n 5)) (if (= n 0) 1 (* n (fact (- n 1))))) 120)))

(define-test "named-let-countdown"
  (lambda () (assert-equal (let loop ((i 3) (acc '()))
                  (if (< i 0)
                      acc
                      (loop (- i 1) (cons i acc))))
                '(0 1 2 3)))) ; Note: cons builds list in reverse order of iteration

(define-test "named-let-access-outer"
  (lambda () (let ((outer 10))
    (assert-equal (let inner ((x 1)) (+ x outer)) 11))))

;; --- COND ---
(define-test "cond-first-clause-true"
  (lambda () (assert-equal (cond (#t 1)
                      (#f 2)
                      (else 3))
                1)))

(define-test "cond-second-clause-true"
  (lambda () (assert-equal (cond (#f 1)
                      ((= 2 2) 2)
                      (else 3))
                2)))

(define-test "cond-else-clause"
  (lambda () (assert-equal (cond (#f 1)
                      ((< 1 0) 2)
                      (else 3))
                3)))

(define-test "cond-no-else-match"
  (lambda () (assert-equal (cond ((= 1 2) 'a)
                      ((= 3 4) 'b))
                '()))) ; Result unspecified if no clause matches, expecting NIL

(define-test "cond-no-else-no-match"
  (lambda () (assert-equal (cond (#f 1)
                      (#f 2))
                '()))) ; Result unspecified, expecting NIL

(define-test "cond-test-is-value"
  (lambda () (assert-equal (cond ((list 1 2)) ; Test evaluates to '(1 2), which is truthy
                      (else 'fallback))
                '(1 2)))) ; R5RS: If body is empty, result is the test value

(define-test "cond-test-is-value-false"
  (lambda () (assert-equal (cond (#f) ; Test is #f
                      (else 'fallback))
                'fallback)))

(define-test "cond-multiple-body-exprs"
  (lambda () (let ((x 0))
    (assert-equal (cond (#t (set! x 1) (+ x 10))
                        (else 99))
                  11)))) ; Result is the last expression in the body

(define-test "cond-truthiness-nil"
  (lambda () (assert-equal (cond ('() 1) ; NIL is truthy
                      (else 2))
                1)))

(define-test "cond-truthiness-zero"
  (lambda () (assert-equal (cond (0 1) ; 0 is truthy
                      (else 2))
                1)))

(define-test "cond-truthiness-list"
  (lambda () (assert-equal (cond ((list) 1) ; Empty list is truthy
                      (else 2))
                1)))

;; --- AND ---
(define-test "and-empty" (lambda () (assert-equal (and) #t)))
(define-test "and-true" (lambda () (assert-equal (and #t) #t)))
(define-test "and-false" (lambda () (assert-equal (and #f) #f)))
(define-test "and-two-true" (lambda () (assert-equal (and #t #t) #t)))
(define-test "and-true-false" (lambda () (assert-equal (and #t #f) #f)))
(define-test "and-false-true" (lambda () (assert-equal (and #f #t) #f))) ; Short-circuits
(define-test "and-returns-last-value" (lambda () (assert-equal (and 1 2 'three) 'three)))
(define-test "and-short-circuit-value" (lambda () (assert-equal (and #f (/ 1 0)) #f))) ; Division by zero should not happen
(define-test "and-truthy-values" (lambda () (assert-equal (and '() 0 "yes") "yes")))

;; --- OR ---
(define-test "or-empty" (lambda () (assert-equal (or) #f)))
(define-test "or-false" (lambda () (assert-equal (or #f) #f)))
(define-test "or-true" (lambda () (assert-equal (or #t) #t)))
(define-test "or-two-false" (lambda () (assert-equal (or #f #f) #f)))
(define-test "or-false-true" (lambda () (assert-equal (or #f #t) #t)))
(define-test "or-true-false" (lambda () (assert-equal (or #t #f) #t))) ; Short-circuits
(define-test "or-returns-first-truthy" (lambda () (assert-equal (or #f 0 'ignored) 0)))
(define-test "or-short-circuit-value" (lambda () (assert-equal (or #t (/ 1 0)) #t))) ; Division by zero should not happen
(define-test "or-all-false" (lambda () (assert-equal (or #f #f #f) #f)))