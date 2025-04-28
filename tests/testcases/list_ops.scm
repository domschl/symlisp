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

;; --- LIST ---
(define-test "list-numbers" (assert-equal (list 1 2 3) '(1 2 3)))
(define-test "list-symbols" (assert-equal (list 'a 'b 'c) '(a b c)))
(define-test "list-mixed" (assert-equal (list 1 'a #t) '(1 a #t)))
(define-test "list-nested" (assert-equal (list 1 (list 2 3) 4) '(1 (2 3) 4)))
(define-test "list-empty" (assert-equal (list) '()))
(define-test "list-eval-args" (assert-equal (list (+ 1 1) (* 2 3)) '(2 6)))