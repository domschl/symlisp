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
(define-test "let-tco-body" ; Last expression in body is tail call
    (letrec* ((f (lambda (n acc) (if (= n 0) acc (f (- n 1) (+ acc n))))))
      (assert-equal (let ((x 10000)) (f x 0)) ; Should not overflow C stack
                    50005000)))

;; --- NAMED LET ---
(define-test "named-let-simple-loop"
  (assert-equal (let loop ((n 5) (acc 1))
                  (if (= n 0) acc (loop (- n 1) (* acc n))))
                120)) ; 5!
(define-test "named-let-tco"
  (assert-equal (let loop ((n 10000) (acc 0)) ; Should not overflow C stack
                  (if (= n 0) acc (loop (- n 1) (+ acc n))))
                50005000))


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