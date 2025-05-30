;;; Symlisp Simple Testing Library
;;; Provides basic testing utilities with one assertion per test

;; Initialize global test counters
(define tests-all-passed #t)
(define tests-run-count 0)
(define tests-passed-count 0)
(define tests-failed-count 0)

;; assert-equal: Compare two values for equality
(define assert-equal
  (lambda (actual expected)
    (let ((passed (equal? actual expected)))
      (if passed
          #t
          (begin
            (display "  ASSERTION FAILED:\n")
            (display "  Got:      ")
            (write actual)
            (display "\n  Expected: ")
            (write expected)
            (display "\n")
            #f)))))

(define assert-true
  (lambda (value) (assert-equal value #t)))
(define assert-false
  (lambda (value) (assert-equal value #f)))

;; define-test: Define and run a named test case with ONE assertion
(define define-test-simple
  (lambda (name assertion)
    ;; Increment the test counter
    (set! tests-run-count (+ tests-run-count 1))
    
    ;(display "Running test: ")
    ;(display name)
    ;(display "\n")
    ;(display "   Assertion: ")
    ;(display assertion)
    ;(display "\n")
    (define result "FAILED")
    (define test-passed #f)
    
    ;; Update global counters based on result
    (if (eq? assertion #t)
        (begin
          (display "Test ") (display name) (display " passed\n")
          ;; Increment the passed test counter
          (set! tests-passed-count (+ tests-passed-count 1))
          (set! result "PASSED")
          (set! test-passed #t))
        (begin
          (display "Test ") (display name) (display " failed\n")
          (set! tests-failed-count (+ tests-failed-count 1))
          (set! tests-all-passed #f)))
    
    ;(display "Test '")
    ;(display name)
    ;(display "' ")
    ;(display result)
    ;(display "\n")
    
    ;; Return the test result
    test-passed))

(define define-test
  (lambda (name test-thunk) ; <<< Changed second arg to test-thunk
    (set! tests-run-count (+ tests-run-count 1))

    ;(display "Running test: ") (display name) (display "\n")

    ;; --- Evaluate the test thunk ---
    (define test-passed (test-thunk)) ; <<< Evaluate the lambda
    ;; -----------------------------

    (define result "FAILED")

    ;; Update global counters based on result
    (if (eq? test-passed #t) ; <<< Check the *result* of the thunk
        (begin
          ;(display "Test passed\n")
          (set! tests-passed-count (+ tests-passed-count 1))
          (set! result "PASSED"))
        (begin
          (display "Test ") (display name) (display " failed\n")
          ;; No need to display assertion here, assert-equal already did
          (set! tests-failed-count (+ tests-failed-count 1))
          (set! tests-all-passed #f)))

    ;(display "Test '") (display name) (display "' ") (display result) (display "\n")

    ;; Return the test result
    test-passed))
    