;;; Tests for Core Special Forms

;; --- QUOTE ---
(define-test-thunked "quote-symbol" (lambda () (assert-equal (quote abc) 'abc)))
(define-test-thunked "quote-number" (lambda () (assert-equal (quote 123) 123)))
(define-test-thunked "quote-list" (lambda () (assert-equal (quote (1 2 3)) '(1 2 3))))
(define-test-thunked "quote-nested-list" (lambda () (assert-equal (quote (1 (2 3) 4)) '(1 (2 3) 4))))
(define-test-thunked "quote-true" (lambda () (assert-equal (quote #t) #t)))
(define-test-thunked "quote-false" (lambda () (assert-equal (quote #f) #f)))
(define-test-thunked "quote-nil" (lambda () (assert-equal (quote ()) '())))

;; --- IF ---
(define-test-thunked "if-true-conseq" (lambda () (assert-equal (if #t 1 2) 1)))
(define-test-thunked "if-false-alt" (lambda () (assert-equal (if #f 1 2) 2)))
(define-test-thunked "if-nil-alt" (lambda () (assert-equal (if '() 1 2) 1))) ; NIL is truthy
(define-test-thunked "if-zero-conseq" (lambda () (assert-equal (if 0 1 2) 1))) ; 0 is truthy
(define-test-thunked "if-empty-list-conseq" (lambda () (assert-equal (if (list) 1 2) 1))) ; Empty list is truthy
(define-test-thunked "if-no-alt-true" (lambda () (assert-equal (if #t 5) 5)))
(define-test-thunked "if-no-alt-false" (lambda () (assert-equal (if #f 5) '()))) ; Result unspecified, using NIL

;; --- DEFINE / SET! ---
(define test-var-1 10)
(define-test-thunked "define-var" (lambda () (assert-equal test-var-1 10)))
(set! test-var-1 20)
(define-test-thunked "set!-var" (lambda () (assert-equal test-var-1 20)))

(define (test-func-1 x) (+ x 1))
(define-test-thunked "define-func" (lambda () (assert-equal (test-func-1 5) 6)))

(define test-func-2 (lambda (y) (* y 2)))
(define-test-thunked "define-lambda" (lambda () (assert-equal (test-func-2 7) 14)))

;; --- LAMBDA ---
(define-test-thunked "lambda-immediate-call" (lambda () (assert-equal ((lambda (x) (+ x 5)) 3) 8)))
(define-test-thunked "lambda-no-args" (lambda () (assert-equal ((lambda () 42)) 42)))
(define closure-test-var 100)
(define closure-test-func (lambda (x) (+ x closure-test-var)))
(define-test-thunked "lambda-closure-capture" (lambda () (assert-equal (closure-test-func 5) 105)))
(set! closure-test-var 200) ; Change captured variable
(define-test-thunked "lambda-closure-capture-after-set" (lambda () (assert-equal (closure-test-func 5) 205))) ; Should see new value

;; --- BEGIN ---
(define begin-test-var 0)
(define-test-thunked "begin-sequence"
  (lambda () (assert-equal (begin (set! begin-test-var 1) (set! begin-test-var 2) (+ begin-test-var 3)) 5)))
(define-test-thunked "begin-var-state" (lambda () (assert-equal begin-test-var 2)))
(define-test-thunked "begin-single-expr" (lambda () (assert-equal (begin 99) 99)))
(define-test-thunked "begin-empty" (lambda () (assert-equal (begin) '()))) ; Result unspecified, using NIL

;; --- LET ---
(define-test-thunked "let-simple" (lambda () (assert-equal (let ((x 1) (y 2)) (+ x y)) 3)))
(define let-outer-var 10)
(define-test-thunked "let-uses-outer-scope" (lambda () (assert-equal (let ((x let-outer-var)) x) 10)))
(define-test-thunked "let-shadows-outer" (lambda () (assert-equal (let ((let-outer-var 5)) let-outer-var) 5)))
(define-test-thunked "let-outer-unaffected" (lambda () (assert-equal let-outer-var 10)))
(define-test-thunked "let-empty-bindings" (lambda () (assert-equal (let () 42) 42)))
(define-test-thunked "let-multi-body" (lambda () (assert-equal (let ((x 1)) (set! x (+ x 1)) x) 2)))

;; --- NAMED LET ---
(define-test-thunked "named-let-factorial"
  (lambda () (assert-equal (let fact ((n 5)) (if (= n 0) 1 (* n (fact (- n 1))))) 120)))

(define-test-thunked "named-let-countdown"
  (lambda () (assert-equal (let loop ((i 3) (acc '()))
                  (if (< i 0)
                      acc
                      (loop (- i 1) (cons i acc))))
                '(0 1 2 3)))) ; Note: cons builds list in reverse order of iteration

(define-test-thunked "named-let-access-outer"
  (lambda () (let ((outer 10))
    (assert-equal (let inner ((x 1)) (+ x outer)) 11))))

;; --- COND ---
(define-test-thunked "cond-first-clause-true"
  (lambda () (assert-equal (cond (#t 1)
                      (#f 2)
                      (else 3))
                1)))

(define-test-thunked "cond-second-clause-true"
  (lambda () (assert-equal (cond (#f 1)
                      ((= 2 2) 2)
                      (else 3))
                2)))

(define-test-thunked "cond-else-clause"
  (lambda () (assert-equal (cond (#f 1)
                      ((< 1 0) 2)
                      (else 3))
                3)))

(define-test-thunked "cond-no-else-match"
  (lambda () (assert-equal (cond ((= 1 2) 'a)
                      ((= 3 4) 'b))
                '()))) ; Result unspecified if no clause matches, expecting NIL

(define-test-thunked "cond-no-else-no-match"
  (lambda () (assert-equal (cond (#f 1)
                      (#f 2))
                '()))) ; Result unspecified, expecting NIL

(define-test-thunked "cond-test-is-value"
  (lambda () (assert-equal (cond ((list 1 2)) ; Test evaluates to '(1 2), which is truthy
                      (else 'fallback))
                '(1 2)))) ; R5RS: If body is empty, result is the test value

(define-test-thunked "cond-test-is-value-false"
  (lambda () (assert-equal (cond (#f) ; Test is #f
                      (else 'fallback))
                'fallback)))

(define-test-thunked "cond-multiple-body-exprs"
  (lambda () (let ((x 0))
    (assert-equal (cond (#t (set! x 1) (+ x 10))
                        (else 99))
                  11)))) ; Result is the last expression in the body

(define-test-thunked "cond-truthiness-nil"
  (lambda () (assert-equal (cond ('() 1) ; NIL is truthy
                      (else 2))
                1)))

(define-test-thunked "cond-truthiness-zero"
  (lambda () (assert-equal (cond (0 1) ; 0 is truthy
                      (else 2))
                1)))

(define-test-thunked "cond-truthiness-list"
  (lambda () (assert-equal (cond ((list) 1) ; Empty list is truthy
                      (else 2))
                1)))

;; --- AND ---
(define-test-thunked "and-empty" (lambda () (assert-equal (and) #t)))
(define-test-thunked "and-true" (lambda () (assert-equal (and #t) #t)))
(define-test-thunked "and-false" (lambda () (assert-equal (and #f) #f)))
(define-test-thunked "and-two-true" (lambda () (assert-equal (and #t #t) #t)))
(define-test-thunked "and-true-false" (lambda () (assert-equal (and #t #f) #f)))
(define-test-thunked "and-false-true" (lambda () (assert-equal (and #f #t) #f))) ; Short-circuits
(define-test-thunked "and-returns-last-value" (lambda () (assert-equal (and 1 2 'three) 'three)))
(define-test-thunked "and-short-circuit-value" (lambda () (assert-equal (and #f (/ 1 0)) #f))) ; Division by zero should not happen
(define-test-thunked "and-truthy-values" (lambda () (assert-equal (and '() 0 "yes") "yes")))

;; --- OR ---
(define-test-thunked "or-empty" (lambda () (assert-equal (or) #f)))
(define-test-thunked "or-false" (lambda () (assert-equal (or #f) #f)))
(define-test-thunked "or-true" (lambda () (assert-equal (or #t) #t)))
(define-test-thunked "or-two-false" (lambda () (assert-equal (or #f #f) #f)))
(define-test-thunked "or-false-true" (lambda () (assert-equal (or #f #t) #t)))
(define-test-thunked "or-true-false" (lambda () (assert-equal (or #t #f) #t))) ; Short-circuits
(define-test-thunked "or-returns-first-truthy" (lambda () (assert-equal (or #f 0 'ignored) 0)))
(define-test-thunked "or-short-circuit-value" (lambda () (assert-equal (or #t (/ 1 0)) #t))) ; Division by zero should not happen
(define-test-thunked "or-all-false" (lambda () (assert-equal (or #f #f #f) #f)))