;;; Tests for Lexical Scoping

(define scope-outer-1 10)

(define-test "scope-global-access" (lambda () (assert-equal scope-outer-1 10)))

;; --- Function Scope ---
(define (scope-func-1 x)
  (+ x scope-outer-1)) ; Access global

(define-test "scope-func-access-global" (lambda () (assert-equal (scope-func-1 5) 15)))

(define (scope-func-2 x)
  (define scope-inner-1 20) ; Define local
  (+ x scope-inner-1))

(define-test "scope-func-define-local" (lambda () (assert-equal (scope-func-2 5) 25)))
;; Cannot test access to scope-inner-1 from outside here easily

(define (scope-func-3 x)
  (let ((scope-outer-1 50)) ; Shadow global
    (+ x scope-outer-1)))

(define-test "scope-func-shadow-global-let" (lambda () (assert-equal (scope-func-3 5) 55)))
(define-test "scope-global-unaffected-by-shadow" (lambda () (assert-equal scope-outer-1 10)))

;; --- Let Scope ---
(define scope-outer-2 100)
(define-test "scope-let-access-outer"
  (lambda () (assert-equal (let ((y 5)) (+ y scope-outer-2)) 105)))

(define-test "scope-let-shadowing"
  (lambda () (assert-equal (let ((scope-outer-2 200)) scope-outer-2) 200)))

(define-test "scope-nested-let"
  (lambda () (assert-equal (let ((x 1))
                  (let ((y 2))
                    (+ x y))) ; Inner let sees outer let's vars
                3)))

(define-test "scope-nested-let-shadowing"
  (lambda () (assert-equal (let ((x 1))
                  (let ((x 2) (y 3)) ; Inner x shadows outer x
                    (+ x y)))
                5)))

;; --- Lambda Closure Scope ---
(define (scope-make-adder x)
  (lambda (y) (+ x y))) ; y is param, x is captured from outer scope

(define scope-add-5 (scope-make-adder 5))
(define scope-add-10 (scope-make-adder 10))

(define-test "scope-closure-1" (lambda () (assert-equal (scope-add-5 3) 8)))
(define-test "scope-closure-2" (lambda () (assert-equal (scope-add-10 3) 13)))
(define-test "scope-closure-independent" (lambda () (assert-equal (scope-add-5 1) 6))) ; Ensure add-10 didn't affect add-5's x

;; --- Set! and Scope ---
(define scope-set-test-var 1)
(define (scope-set-func)
  (set! scope-set-test-var 2)) ; Modify global

(define-test "scope-set!-global-initial" (lambda () (assert-equal scope-set-test-var 1)))
(scope-set-func) ; Call function to modify global
(define-test "scope-set!-global-modified" (lambda () (assert-equal scope-set-test-var 2)))

(define-test "scope-set!-let-var"
  (lambda () (assert-equal (let ((z 10))
                  (set! z 11)
                  z)
                11)))

(define scope-set-outer-let 50)
(define-test "scope-set!-outer-var-from-lambda"
  (lambda () 
  (let ((setter (lambda () (set! scope-set-outer-let 51))))
    (setter) ; Call lambda to modify outer variable
    (assert-equal scope-set-outer-let 51))))