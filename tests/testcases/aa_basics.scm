;;; Basic Language Feature Tests using define-test-simple
;;; This file tests fundamental language features that the test framework itself relies on
;;; To avoid circular dependencies, these tests use the simple testing mechanism

;;; --- Lambda Expression Tests ---

(define test-lambda-result (lambda (x) (* x 2)))
(define-test-simple "lambda-basic-evaluation" 
  (equal? (test-lambda-result 5) 10))

(define-test-simple "lambda-direct-call" 
  (equal? ((lambda (x) (+ x 1)) 5) 6))

(define-test-simple "lambda-no-args"
  (equal? ((lambda () 42)) 42))

(define-test-simple "lambda-closure-basic"
  (let ((x 10))
    (equal? ((lambda () x)) 10)))

(define-test-simple "lambda-multi-args"
  (equal? ((lambda (a b c) (+ a (* b c))) 1 2 3) 7))

;;; --- Equal? Tests (used by assert-equal) ---

(define-test-simple "equal?-numbers" 
  (equal? 5 5))

(define-test-simple "equal?-numbers-fail" 
  (not (equal? 5 6)))

(define-test-simple "equal?-strings" 
  (equal? "hello" "hello"))

(define-test-simple "equal?-strings-fail" 
  (not (equal? "hello" "world")))

(define-test-simple "equal?-symbols" 
  (equal? 'symbol 'symbol))

(define-test-simple "equal?-symbols-fail" 
  (not (equal? 'symbol1 'symbol2)))

(define-test-simple "equal?-lists" 
  (equal? '(1 2 3) '(1 2 3)))

(define-test-simple "equal?-lists-fail" 
  (not (equal? '(1 2 3) '(1 2 4))))

(define-test-simple "equal?-empty-lists" 
  (equal? '() '()))

(define-test-simple "equal?-nested-lists" 
  (equal? '(1 (2 3) 4) '(1 (2 3) 4)))

;;; --- Boolean Tests ---

(define-test-simple "boolean-true" 
  (eq? #t #t))

(define-test-simple "boolean-false" 
  (eq? #f #f))

(define-test-simple "not-true" 
  (eq? (not #t) #f))

(define-test-simple "not-false" 
  (eq? (not #f) #t))

(define-test-simple "if-true-branch" 
  (equal? (if #t 1 2) 1))

(define-test-simple "if-false-branch" 
  (equal? (if #f 1 2) 2))

;;; --- Let Binding Tests ---

(define-test-simple "let-simple-binding" 
  (equal? (let ((x 5)) x) 5))

(define-test-simple "let-expression-binding" 
  (equal? (let ((x (+ 2 3))) x) 5))

(define-test-simple "let-multiple-bindings" 
  (equal? (let ((x 5) (y 10)) (+ x y)) 15))

(define-test-simple "let-nested" 
  (equal? (let ((x 5)) (let ((y 10)) (+ x y))) 15))

(define-test-simple "let-shadowing" 
  (equal? (let ((x 5)) (let ((x 10)) x)) 10))

;;; --- Set! Tests ---

(define set-test-var 5)
(define-test-simple "set!-basic" 
  (begin (set! set-test-var 10) (equal? set-test-var 10)))

(define-test-simple "set!-in-let" 
  (let ((x 5))
    (begin (set! x 10) (equal? x 10))))

;;; --- Begin Tests ---

(define-test-simple "begin-basic" 
  (equal? (begin 1 2 3) 3))

(define-test-simple "begin-with-set!" 
  (let ((x 5))
    (equal? (begin (set! x 10) (+ x 1)) 11)))

;;; --- Quote Tests ---

(define-test-simple "quote-symbol" 
  (equal? 'symbol 'symbol))

(define-test-simple "quote-list" 
  (equal? '(1 2 3) '(1 2 3)))

;;; --- Display Tests (used by testing framework) ---

(define-test-simple "display-return-value" 
  (equal? (display "Test output") '()))

(define-test-simple "write-return-value" 
  (equal? (write "Test output") '()))

;;; Print a summary of these basic tests
(display "\nBasic language feature tests complete.\n")
(display "Run: ") (display tests-run-count)
(display ", Passed: ") (display tests-passed-count)
(display ", Failed: ") (display tests-failed-count)
(display "\n")
(display "All tests passed: ") (display tests-all-passed)
(display "\n\n")
