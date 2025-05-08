;;; Tests for List Operations

(define test-list-1 '(1 2 3 4))
(define test-list-2 '(a b c))
(define test-list-3 '((1 2) (3 4) (5 6))) ; Extended list 3 for caddr
(define test-list-4 '())

;; --- CAR ---
(define-test "car-simple" (assert-equal (car test-list-1) 1))
(define-test "car-symbol" (assert-equal (car test-list-2) 'a))
(define-test "car-nested" (assert-equal (car test-list-3) '(1 2)))

;; --- CDR ---
(define-test "cdr-simple" (assert-equal (cdr test-list-1) '(2 3 4)))
(define-test "cdr-symbol" (assert-equal (cdr test-list-2) '(b c)))
(define-test "cdr-nested" (assert-equal (cdr test-list-3) '((3 4) (5 6))))
(define-test "cdr-last-pair" (assert-equal (cdr '(4)) '()))

;; --- CXR ---
(define-test "cadr-simple" (assert-equal (cadr test-list-1) 2)) ; (car (cdr '(1 2 3 4))) -> (car '(2 3 4)) -> 2
(define-test "cadr-symbol" (assert-equal (cadr test-list-2) 'b)) ; (car (cdr '(a b c))) -> (car '(b c)) -> b
(define-test "cadr-nested" (assert-equal (cadr test-list-3) '(3 4))) ; (car (cdr '((1 2) (3 4) (5 6)))) -> (car '((3 4) (5 6))) -> (3 4)

(define-test "caar-simple" (assert-equal (caar test-list-3) 1)) ; (car (car '((1 2) (3 4) (5 6)))) -> (car '(1 2)) -> 1
(define-test "caar-symbol" (assert-equal (caar test-list-2) '())) 
(define-test "caar-nested" (assert-equal (caar '((1 2) (3 4))) 1)) ; (car (car '((1 2) (3 4)))) -> (car '(1 2)) -> 1

(define-test "cddr-simple" (assert-equal (cddr test-list-1) '(3 4))) ; (cdr (cdr '(1 2 3 4))) -> (cdr '(2 3 4)) -> (3 4)
(define-test "cddr-symbol" (assert-equal (cddr test-list-2) '(c))) ; (cdr (cdr '(a b c))) -> (cdr '(b c)) -> (c)
(define-test "cddr-nested" (assert-equal (cddr test-list-3) '((5 6)))) ; (cdr (cdr '((1 2) (3 4) (5 6)))) -> (cdr '((3 4) (5 6))) -> ((5 6))

(define-test "caddr-simple" (assert-equal (caddr test-list-1) 3)) ; (car (cdr (cdr '(1 2 3 4)))) -> (car '(3 4)) -> 3
(define-test "caddr-symbol" (assert-equal (caddr test-list-2) 'c)) ; (car (cdr (cdr '(a b c)))) -> (car '(c)) -> c
(define-test "caddr-nested" (assert-equal (caddr test-list-3) '(5 6))) ; (car (cdr (cdr '((1 2) (3 4) (5 6))))) -> (car '((5 6))) -> (5 6)

(define test-list-3 '((1 2) (3 4))) ; Reduced list to 2 again

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

;; --- LENGTH (from stdsymlisp/lists.scm if not builtin) ---
;; Assuming length tests from above are for a builtin or previously defined one.
;; If length was just added to lists.scm, these are the primary tests for it.
(define-test "length-new-simple" (assert-equal (length '(a b c d)) 4))
(define-test "length-new-nested" (assert-equal (length '((1 2) (3 4))) 2))
(define-test "length-new-empty" (assert-equal (length '()) 0))
(define-test "length-new-single" (assert-equal (length '(1)) 1))
(define-test "length-new-dotted-pair" (assert-equal (length '(1 . 2)) 1)) ; Length counts pairs up to non-pair cdr
(define-test "length-new-improper-list" (assert-equal (length '(1 2 . 3)) 2))

;; --- LIST-TAKE ---
(define-test "list-take-simple" (assert-equal (list-take '(a b c d e) 3) '(a b c)))
(define-test "list-take-zero" (assert-equal (list-take '(a b c) 0) '()))
(define-test "list-take-more-than-length" (assert-equal (list-take '(a b) 5) '(a b)))
(define-test "list-take-exact-length" (assert-equal (list-take '(a b c) 3) '(a b c)))
(define-test "list-take-from-empty" (assert-equal (list-take '() 3) '()))
(define-test "list-take-negative-k" (assert-equal (list-take '(a b c) -1) '()))
(define-test "list-take-non-destructive"
  (let ((original '(1 2 3 4)))
    (list-take original 2)
    (assert-equal original '(1 2 3 4))))

;; --- LIST-DROP ---
(define-test "list-drop-simple" (assert-equal (list-drop '(a b c d e) 2) '(c d e)))
(define-test "list-drop-zero" (assert-equal (list-drop '(a b c) 0) '(a b c)))
(define-test "list-drop-more-than-length" (assert-equal (list-drop '(a b) 5) '()))
(define-test "list-drop-exact-length" (assert-equal (list-drop '(a b c) 3) '()))
(define-test "list-drop-from-empty" (assert-equal (list-drop '() 3) '()))
(define-test "list-drop-negative-k" (assert-equal (list-drop '(a b c) -1) '(a b c)))
(define-test "list-drop-non-destructive"
  (let ((original '(1 2 3 4)))
    (list-drop original 2)
    (assert-equal original '(1 2 3 4))))

;; --- MERGE-SORTED-LISTS ---
(define num-pred <)
(define-test "merge-sorted-empty-both" (assert-equal (merge-sorted-lists '() '() num-pred) '()))
(define-test "merge-sorted-empty-left" (assert-equal (merge-sorted-lists '() '(1 3 5) num-pred) '(1 3 5)))
(define-test "merge-sorted-empty-right" (assert-equal (merge-sorted-lists '(2 4 6) '() num-pred) '(2 4 6)))
(define-test "merge-sorted-interleaved" (assert-equal (merge-sorted-lists '(1 3 5) '(2 4 6) num-pred) '(1 2 3 4 5 6)))
(define-test "merge-sorted-list1-first" (assert-equal (merge-sorted-lists '(1 2 3) '(4 5 6) num-pred) '(1 2 3 4 5 6)))
(define-test "merge-sorted-list2-first" (assert-equal (merge-sorted-lists '(4 5 6) '(1 2 3) num-pred) '(1 2 3 4 5 6)))
(define-test "merge-sorted-with-duplicates" (assert-equal (merge-sorted-lists '(1 3 3 5) '(2 3 4 6) num-pred) '(1 2 3 3 3 4 5 6)))
(define-test "merge-sorted-one-each" (assert-equal (merge-sorted-lists '(1) '(2) num-pred) '(1 2)))
(define-test "merge-sorted-one-each-reverse-pred" (assert-equal (merge-sorted-lists '(2) '(1) num-pred) '(1 2)))

;; --- LIST-SORT ---
(define-test "list-sort-empty" (assert-equal (list-sort num-pred '()) '()))
(define-test "list-sort-single" (assert-equal (list-sort num-pred '(1)) '(1)))
(define-test "list-sort-sorted" (assert-equal (list-sort num-pred '(1 2 3 4 5)) '(1 2 3 4 5)))
(define-test "list-sort-reverse-sorted" (assert-equal (list-sort num-pred '(5 4 3 2 1)) '(1 2 3 4 5)))
(define-test "list-sort-unsorted" (assert-equal (list-sort num-pred '(3 1 4 1 5 9 2 6)) '(1 1 2 3 4 5 6 9)))
(define-test "list-sort-with-duplicates" (assert-equal (list-sort num-pred '(5 2 8 2 5 1)) '(1 2 2 5 5 8)))
(define-test "list-sort-non-destructive"
  (let ((original '(3 1 2)))
    (list-sort num-pred original)
    (assert-equal original '(3 1 2))))
(define symbol-pred (lambda (s1 s2) (string<? (symbol->string s1) (symbol->string s2))))
(define-test "list-sort-symbols" (assert-equal (list-sort symbol-pred '(c a b d)) '(a b c d)))
(define term<-pred term<?) ; Assuming term<? is available from symbolics
(define-test "list-sort-terms"
  (assert-equal (list-sort term<-pred '(b 10 (* x y) a 5 (+ 1 z)))
                '(5 10 a b (* x y) (+ 1 z)))) ; Example order, actual depends on expr->string for compounds
