;; --- LET ---
(define-test "let-simple" (assert-equal (let ((x 1)) x) 1))
(define-test "let-two-vars" (assert-equal (let ((x 1) (y 2)) (+ x y)) 3))
(define-test "let-shadowing" (assert-equal (let ((x 1)) (let ((x 2)) x)) 2))
(define-test "let-outer-scope" (assert-equal (let ((x 1)) (let ((y 2)) (+ x y))) 3))
;(define-test "let-init-order" ; Initializers evaluated before binding
;  (let ((x 1) (y x)) ; y should see outer x if defined, error otherwise
;    (assert-error y "Unbound variable: x."))) ; Assuming no global x
(define-test "let-empty-bindings" (assert-equal (let () 5) 5))
(define-test "let-multiple-body" (assert-equal (let ((x 0)) (set! x (+ x 1)) x) 1))

;; --- NAMED LET ---
(define-test "named-let-simple-loop"
  (assert-equal (let loop ((n 5) (acc 1))
                  (if (= n 0) acc (loop (- n 1) (* acc n))))
                120)) ; 5!
(define-test "named-let-tco"
  (assert-equal (let loop ((n 10000) (acc 0)) ; Should not overflow C stack
                  (if (= n 0) acc (loop (- n 1) (+ acc n))))
                50005000))

;; --- LET* ---
(define-test "let*-simple" (assert-equal (let* ((x 1)) x) 1))
(define-test "let*-sequential" (assert-equal (let* ((x 1) (y x)) y) 1))
(define-test "let*-sequential-expr" (assert-equal (let* ((x 1) (y (+ x 1))) y) 2))
(define-test "let*-shadowing" (assert-equal (let* ((x 1) (x (+ x 1))) x) 2))
(define-test "let*-empty-bindings" (assert-equal (let* () 5) 5))
(define-test "let*-multiple-body" (assert-equal (let* ((x 0)) (set! x (+ x 1)) x) 1))

;; --- LETREC* ---
(define-test "letrec*-simple" (assert-equal (letrec* ((x 1)) x) 1))
(define-test "letrec*-mutual-recursion"
  (assert-equal (letrec* ((is-even? (lambda (n) (if (= n 0) #t (is-odd? (- n 1)))))
                          (is-odd? (lambda (n) (if (= n 0) #f (is-even? (- n 1))))))
                  (is-odd? 5))
                #t))
(define-test "letrec*-sequential-eval" ; Like let*, later bindings see earlier ones
  (assert-equal (letrec* ((x 1) (y x)) y) 1))
(define-test "letrec*-forward-ref-in-lambda" ; Lambda body evaluated later
  (assert-equal (letrec* ((f (lambda () y)) (y 2)) (f)) 2))
;(define-test "letrec*-forward-ref-in-init" ; R7RS disallows this for letrec*
;  (assert-error (letrec* ((x y) (y 1)) x) "Cannot reference variable before initialization")) ; Or similar error
(define-test "letrec*-empty-bindings" (assert-equal (letrec* () 5) 5))
(define-test "letrec*-multiple-body" (assert-equal (letrec* ((x 0)) (set! x (+ x 1)) x) 1))
(define-test "letrec*-tco-body" ; Last expression in body is tail call
    (letrec* ((f (lambda (n acc) (if (= n 0) acc (f (- n 1) (+ acc n))))))
      (assert-equal (letrec* ((x 10000)) (f x 0)) ; Should not overflow C stack
                    50005000)))

;; --- Nested Defines ---
(define-test "nested-define-simple-var"
  (assert-equal (let ((x 1)) (define y (+ x 1)) y) 2))
(define-test "nested-define-simple-func"
  (assert-equal (let ((x 1)) (define (get-x) x) (get-x)) 1))
(define-test "nested-define-sees-outer-let"
  (assert-equal (let ((a 10))
                  (define (add-a b) (+ a b))
                  (add-a 5))
                15))
(define-test "nested-define-in-lambda"
  (assert-equal (let ((f (lambda (x)
                           (define y (+ x 1))
                           y)))
                  (f 5))
                6))
(define-test "nested-define-mutual-recursion"
  (assert-equal (let ()
                  (define (is-even? n) (if (= n 0) #t (is-odd? (- n 1))))
                  (define (is-odd? n) (if (= n 0) #f (is-even? (- n 1))))
                  (is-odd? 7))
                #t))
(define-test "nested-define-sequential-eval" ; Later defines see earlier ones
  (assert-equal (let () (define x 1) (define y x) y) 1))
(define-test "nested-define-forward-ref-in-lambda" ; Lambda body evaluated later
  (assert-equal (let () (define (f) y) (define y 2) (f)) 2))
;(define-test "nested-define-forward-ref-in-init" ; Disallowed
;  (assert-error (let () (define x y) (define y 1) x) "Cannot reference variable before initialization"))
(define-test "nested-define-only-at-start" ; Defines must appear before other expressions
  (assert-equal (let ((x 1))
                  x ; Expression before define
                  (define y 2) ; This define might be ignored or error depending on strictness
                  y) ; Expect 1 if define ignored, error if define disallowed here
                2)) ; NOT assuming define is ignored if not at start (deviation from R7RS)
(define-test "nested-define-tco"
  (let ()
    (define (sum n acc)
      (if (= n 0) acc (sum (- n 1) (+ n acc))))
    (assert-equal (sum 10000 0) 50005000))) ; Should not overflow C stack

;; --- Variadic Functions / Lambdas ---

(define-test "lambda-variadic-dot"
  (let ((f (lambda (a b . rest) (list a b rest))))
    (assert-equal (f 1 2) '(1 2 ()))
    (assert-equal (f 1 2 3) '(1 2 (3)))
    (assert-equal (f 1 2 3 4 5) '(1 2 (3 4 5)))))

;(define-test "lambda-variadic-dot-arity-fail" ; Need at least 2 args
;  (let ((f (lambda (a b . rest) rest)))
;    (assert-error (f 1) "Apply: Mismatched argument count")))

(define-test "lambda-variadic-symbol"
  (let ((f (lambda args args))) ; Collect all args into 'args'
    (assert-equal (f) '())
    (assert-equal (f 1) '(1))
    (assert-equal (f 1 2 3) '(1 2 3))))

(define-test "lambda-variadic-zero-args"
  (let ((f (lambda () 0)))
    (assert-equal (f) 0)))
;    (assert-error (f 1) "Apply: Mismatched argument count")))

(define-test-thunked "define-variadic-dot"
  (lambda ()
  (define (my-list a . items) (cons a items))
  (assert-equal (my-list 1) '(1)) ; items is ()
  (assert-equal (my-list 1 2 3) '(1 2 3)))) ; items is (2 3)

(define-test-thunked "define-variadic-symbol"
  (lambda ()
  (define (sum-all nums) (apply + nums)) ; Assumes apply and + work
  (assert-equal (sum-all '(1 2 3 4)) 10))) ; Note: This tests passing a list

 ; Test apply on a variadic lambda
(define-test-thunked "apply-with-variadic"
  (lambda ()
  (let ((f (lambda (a . rest) (list a rest))))
    (assert-equal (apply f '(1 2 3)) '(1 (2 3))))))

;; Check interaction with builtins (assuming + is variadic)
(define-test-thunked "builtin-variadic-plus"
  (lambda ()
  (assert-equal (+) 0)
  (assert-equal (+ 1) 1)
  (assert-equal (+ 1 2 3 4 5) 15)))

;; Check interaction with builtins (assuming list is variadic)
(define-test-thunked "builtin-variadic-list"
  (lambda ()
  (assert-equal (list) '())
  (assert-equal (list 1) '(1))
  (assert-equal (list 1 2 3) '(1 2 3))))

;; --- More Apply Tests ---

(define-test-thunked "apply-no-intermediate-args"
  (lambda ()
    (assert-equal (apply + '(1 2 3 4)) 10)))

(define-test-thunked "apply-with-intermediate-args"
  (lambda ()
    (assert-equal (apply + 1 2 '(3 4 5)) 15)))

(define-test-thunked "apply-with-empty-list"
  (lambda ()
    (assert-equal (apply + 1 2 '()) 3)))

(define-test-thunked "apply-only-proc-and-empty-list"
  (lambda ()
    (assert-equal (apply + '()) 0)))

(define-test-thunked "apply-on-user-lambda"
  (lambda ()
    (let ((add (lambda (x y) (+ x y))))
      (assert-equal (apply add '(3 4)) 7))))

(define-test-thunked "apply-on-user-lambda-intermediate"
  (lambda ()
    (let ((add3 (lambda (x y z) (+ x y z))))
      (assert-equal (apply add3 1 '(2 3)) 6))))
