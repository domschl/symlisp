;;; Tests for List Operations

(define test-list-1 '(1 2 3 4))
(define test-list-2 '(a b c))
(define test-list-3 '((1 2) (3 4)))
(define test-list-4 '())

;; --- CAR ---
(define-test "car-simple" (assert-equal (car test-list-1) 1))
(define-test "car-symbol" (assert-equal (car test-list-2) 'a))
(define-test "car-nested" (assert-equal (car test-list-3) '(1 2)))

;; --- CDR ---
(define-test "cdr-simple" (assert-equal (cdr test-list-1) '(2 3 4)))
(define-test "cdr-symbol" (assert-equal (cdr test-list-2) '(b c)))
(define-test "cdr-nested" (assert-equal (cdr test-list-3) '((3 4))))
(define-test "cdr-last-pair" (assert-equal (cdr '(4)) '()))

;; --- CONS ---
(define-test "cons-number-list" (assert-equal (cons 0 test-list-1) '(0 1 2 3 4)))
(define-test "cons-symbol-list" (assert-equal (cons 'x test-list-2) '(x a b c)))
(define-test "cons-list-list" (assert-equal (cons '(0) test-list-3) '((0) (1 2) (3 4))))
(define-test "cons-onto-empty" (assert-equal (cons 1 '()) '(1)))
(define-test "cons-make-dotted" (assert-equal (cons 1 2) '(1 . 2)))

;; --- CONS* ---
(define-test "cons*-single-arg" (assert-equal (cons* 1) 1))
(define-test "cons*-two-args-proper" (assert-equal (cons* 1 2) '(1 . 2))) ; Note: R7RS says (cons* 1 2) -> (1 . 2)
(define-test "cons*-two-args-list" (assert-equal (cons* 1 '(2 3)) '(1 2 3)))

;; --- LIST ---
(define-test "list-numbers" (assert-equal (list 1 2 3) '(1 2 3)))
(define-test "list-symbols" (assert-equal (list 'a 'b 'c) '(a b c)))
(define-test "list-mixed" (assert-equal (list 1 'a #t) '(1 a #t)))
(define-test "list-nested" (assert-equal (list 1 (list 2 3) 4) '(1 (2 3) 4)))
(define-test "list-empty" (assert-equal (list) '()))
(define-test "list-eval-args" (assert-equal (list (+ 1 1) (* 2 3)) '(2 6)))

;; --- LENGTH ---
(define-test "length-simple" (assert-equal (length test-list-1) 4))
(define-test "length-nested" (assert-equal (length test-list-3) 2)) ; Length is top-level
(define-test "length-empty" (assert-equal (length test-list-4) 0))
(define-test "length-single" (assert-equal (length '(1)) 1))

;; --- REVERSE ---
(define-test "reverse-simple" (assert-equal (reverse test-list-1) '(4 3 2 1)))
(define-test "reverse-symbols" (assert-equal (reverse test-list-2) '(c b a)))
(define-test "reverse-nested" (assert-equal (reverse test-list-3) '((3 4) (1 2))))
(define-test "reverse-empty" (assert-equal (reverse test-list-4) '()))
(define-test "reverse-single" (assert-equal (reverse '(1)) '(1)))
(define reverse-original '(x y z))
(reverse reverse-original) ; Call reverse
(define-test "reverse-non-destructive" (assert-equal reverse-original '(x y z))) ; Check original is unchanged

;; --- APPEND ---
(define-test "append-two-lists" (assert-equal (append '(1 2) '(3 4)) '(1 2 3 4)))
(define-test "append-multiple-lists" (assert-equal (append '(1) '(2 3) '(4 5 6)) '(1 2 3 4 5 6)))
(define-test "append-with-empty" (assert-equal (append '(1 2) '() '(3 4)) '(1 2 3 4)))
(define-test "append-to-empty" (assert-equal (append '() '(1 2)) '(1 2)))
(define-test "append-from-empty" (assert-equal (append '(1 2) '()) '(1 2)))
(define-test "append-all-empty" (assert-equal (append '() '() '()) '()))
(define-test "append-no-args" (assert-equal (append) '()))
(define-test "append-single-arg" (assert-equal (append '(1 2 3)) '(1 2 3)))
(define-test "append-last-arg-not-list" (assert-equal (append '(1 2) 3) '(1 2 . 3)))
(define-test "append-last-arg-symbol" (assert-equal (append '(a b) 'c) '(a b . c)))
(define-test "append-makes-copies"
  (let ((l1 '(1 2)))
    (let ((l2 (append l1 '(3))))
      (set-car! l1 99) ; Modify original list
      (assert-equal l2 '(1 2 3))))) ; Appended list should not see the change

;; --- SET-CAR! ---
(define set-car-test-list (list 'a 'b 'c))
(define-test "set-car!-return-value" (assert-equal (set-car! set-car-test-list 'x) '())) ; Returns unspecified (NIL)
(define-test "set-car!-effect" (assert-equal set-car-test-list '(x b c))) ; Check list was modified
(define-test "set-car!-modify-again" (assert-equal (set-car! (cdr set-car-test-list) 'y) '()))
(define-test "set-car!-effect-again" (assert-equal set-car-test-list '(x y c)))

;; --- SET-CDR! ---
(define set-cdr-test-list (list 1 2 3))
(define-test "set-cdr!-return-value" (assert-equal (set-cdr! set-cdr-test-list '(9 8)) '())) ; Returns unspecified (NIL)
(define-test "set-cdr!-effect" (assert-equal set-cdr-test-list '(1 9 8))) ; Check list was modified
(define-test "set-cdr!-make-dotted" (assert-equal (set-cdr! (cdr set-cdr-test-list) 99) '()))
(define-test "set-cdr!-effect-dotted" (assert-equal set-cdr-test-list '(1 9 . 99)))
(define set-cdr-test-list-2 (list 'p 'q 'r))
(define-test "set-cdr!-last-pair" (assert-equal (set-cdr! (cdr set-cdr-test-list-2) '()) '())) ; Make (cdr '(q r)) -> '()
(define-test "set-cdr!-effect-last-pair" (assert-equal set-cdr-test-list-2 '(p q))) ; List is now shorter
