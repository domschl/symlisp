;;; Tests for List Operations

(define test-list-1 '(1 2 3 4))
(define test-list-2 '(a b c))
(define test-list-3 '((1 2) (3 4) (5 6))) ; Extended list 3 for caddr
(define test-list-4 '())

;; --- CAR ---
(define-test-thunked "car-simple" (lambda () (assert-equal (car test-list-1) 1)))
(define-test-thunked "car-symbol" (lambda () (assert-equal (car test-list-2) 'a)))
(define-test-thunked "car-nested" (lambda () (assert-equal (car test-list-3) '(1 2))))

;; --- CDR ---
(define-test-thunked "cdr-simple" (lambda () (assert-equal (cdr test-list-1) '(2 3 4))))
(define-test-thunked "cdr-symbol" (lambda () (assert-equal (cdr test-list-2) '(b c))))
(define-test-thunked "cdr-nested" (lambda () (assert-equal (cdr test-list-3) '((3 4) (5 6)))))
(define-test-thunked "cdr-last-pair" (lambda () (assert-equal (cdr '(4)) '())))

;; --- CXR ---
(define-test-thunked "cadr-simple" (lambda () (assert-equal (cadr test-list-1) 2))) ; (car (cdr '(1 2 3 4))) -> (car '(2 3 4)) -> 2
(define-test-thunked "cadr-symbol" (lambda () (assert-equal (cadr test-list-2) 'b))) ; (car (cdr '(a b c))) -> (car '(b c)) -> b
(define-test-thunked "cadr-nested" (lambda () (assert-equal (cadr test-list-3) '(3 4)))) ; (car (cdr '((1 2) (3 4) (5 6)))) -> (car '((3 4) (5 6))) -> (3 4)

(define-test-thunked "caar-simple" (lambda () (assert-equal (caar test-list-3) 1))) ; (car (car '((1 2) (3 4) (5 6)))) -> (car '(1 2)) -> 1
(define-test-thunked "caar-symbol" (lambda () (assert-equal (caar test-list-2) '()))) 
(define-test-thunked "caar-nested" (lambda () (assert-equal (caar '((1 2) (3 4))) 1))) ; (car (car '((1 2) (3 4)))) -> (car '(1 2)) -> 1

(define-test-thunked "cddr-simple" (lambda () (assert-equal (cddr test-list-1) '(3 4)))) ; (cdr (cdr '(1 2 3 4))) -> (cdr '(2 3 4)) -> (3 4)
(define-test-thunked "cddr-symbol" (lambda () (assert-equal (cddr test-list-2) '(c)))) ; (cdr (cdr '(a b c))) -> (cdr '(b c)) -> (c)
(define-test-thunked "cddr-nested" (lambda () (assert-equal (cddr test-list-3) '((5 6))))) ; (cdr (cdr '((1 2) (3 4) (5 6)))) -> (cdr '((3 4) (5 6))) -> ((5 6))

(define-test-thunked "caddr-simple" (lambda () (assert-equal (caddr test-list-1) 3))) ; (car (cdr (cdr '(1 2 3 4)))) -> (car '(3 4)) -> 3
(define-test-thunked "caddr-symbol" (lambda () (assert-equal (caddr test-list-2) 'c))) ; (car (cdr (cdr '(a b c)))) -> (car '(c)) -> c
(define-test-thunked "caddr-nested" (lambda () (assert-equal (caddr test-list-3) '(5 6)))) ; (car (cdr (cdr '((1 2) (3 4) (5 6))))) -> (car '((5 6))) -> (5 6)

(define test-list-3 '((1 2) (3 4))) ; Reduced list to 2 again

;; --- CONS ---
(define-test-thunked "cons-number-list" (lambda () (assert-equal (cons 0 test-list-1) '(0 1 2 3 4))))
(define-test-thunked "cons-symbol-list" (lambda () (assert-equal (cons 'x test-list-2) '(x a b c))))
(define-test-thunked "cons-list-list" (lambda () (assert-equal (cons '(0) test-list-3) '((0) (1 2) (3 4)))))
(define-test-thunked "cons-onto-empty" (lambda () (assert-equal (cons 1 '()) '(1))))
(define-test-thunked "cons-make-dotted" (lambda () (assert-equal (cons 1 2) '(1 . 2))))

;; --- CONS* ---
(define-test-thunked "cons*-single-arg" (lambda () (assert-equal (cons* 1) 1)))
(define-test-thunked "cons*-two-args-proper" (lambda () (assert-equal (cons* 1 2) '(1 . 2)))) ; Note: R7RS says (cons* 1 2) -> (1 . 2)
(define-test-thunked "cons*-two-args-list" (lambda () (assert-equal (cons* 1 '(2 3)) '(1 2 3))))

;; --- LIST ---
(define-test-thunked "list-numbers" (lambda () (assert-equal (list 1 2 3) '(1 2 3))))
(define-test-thunked "list-symbols" (lambda () (assert-equal (list 'a 'b 'c) '(a b c))))
(define-test-thunked "list-mixed" (lambda () (assert-equal (list 1 'a #t) '(1 a #t))))
(define-test-thunked "list-nested" (lambda () (assert-equal (list 1 (list 2 3) 4) '(1 (2 3) 4))))
(define-test-thunked "list-empty" (lambda () (assert-equal (list) '())))
(define-test-thunked "list-eval-args" (lambda () (assert-equal (list (+ 1 1) (* 2 3)) '(2 6))))

;; --- LENGTH ---
(define-test-thunked "length-simple" (lambda () (assert-equal (length test-list-1) 4)))
(define-test-thunked "length-nested" (lambda () (assert-equal (length test-list-3) 2))) ; Length is top-level
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

;; --- COUNT ---
(define-test "count-empty-list"
  (assert-equal (count number? '()) 0))
(define-test "count-all-match"
  (assert-equal (count number? '(1 2 3)) 3))
(define-test "count-some-match"
  (assert-equal (count even? '(1 2 3 4 5 6)) 3))
(define-test "count-no-match"
  (assert-equal (count symbol? '(1 2 3)) 0))
(define-test "count-symbols"
  (assert-equal (count symbol? '(a b 1 c 2)) 3))
(define-test "count-specific-value"
  (assert-equal (count (lambda (x) (equal? x 'a)) '(a b a c a d)) 3))
(define-test "count-nested-lists-shallow"
  (assert-equal (count list? '((1) 2 (3 4) 5)) 2))

;; --- REMOVE-DUPLICATES ---
(define-test "remove-duplicates-empty-list"
  (assert-equal (remove-duplicates '()) '()))
(define-test "remove-duplicates-no-duplicates"
  (assert-equal (remove-duplicates '(1 2 3 a b)) '(1 2 3 a b)))
(define-test "remove-duplicates-numbers"
  (assert-equal (remove-duplicates '(1 2 1 3 2 2 4 1)) '(1 2 3 4)))
(define-test "remove-duplicates-symbols"
  (assert-equal (remove-duplicates '(a b a c b b d a)) '(a b c d)))
(define-test "remove-duplicates-mixed-types"
  (assert-equal (remove-duplicates '(1 a 1 b a 2 1 c)) '(1 a b 2 c)))
(define-test "remove-duplicates-all-same"
  (assert-equal (remove-duplicates '(x x x x x)) '(x)))
(define-test "remove-duplicates-single-element"
  (assert-equal (remove-duplicates '(5)) '(5)))
(define-test "remove-duplicates-with-lists"
  (assert-equal (remove-duplicates '((1 2) (3 4) (1 2) (5 6) (3 4)))
                '((1 2) (3 4) (5 6))))
(define-test "remove-duplicates-preserves-order"
  (assert-equal (remove-duplicates '(c a b a c d b)) '(c a b d)))

;; --- FIND-IF ---
(define-test "find-if-empty-list"
  (assert-equal (find-if number? '()) #f))
(define-test "find-if-number-present"
  (assert-equal (find-if number? '(a b 3 c d)) 3))
(define-test "find-if-first-element-matches"
  (assert-equal (find-if symbol? '(x 1 2)) 'x))
(define-test "find-if-last-element-matches"
  (assert-equal (find-if (lambda (x) (equal? x 'z)) '(a b c z)) 'z))
(define-test "find-if-no-match"
  (assert-equal (find-if list? '(1 2 3 4)) #f))
(define-test "find-if-list-present"
  (assert-equal (find-if list? '(1 (2 3) 4)) '(2 3)))
(define-test "find-if-predicate-returns-specific-true-value"
  (assert-equal (find-if (lambda (x) (and (number? x) (> x 5))) '(1 2 6 3 7)) 6))
(define-test "find-if-on-list-of-booleans-find-true"
  (assert-equal (find-if (lambda (x) (eq? x #t)) '(#f #f #t #f)) #t))
(define-test "find-if-on-list-of-booleans-find-false"
  (assert-equal (find-if (lambda (x) (eq? x #f)) '(#t #t #f #t)) #f))


;; --- REMOVE (first occurrence) ---
(define-test "remove-empty-list"
  (assert-equal (remove 'a '()) '()))
(define-test "remove-item-present-numbers"
  (assert-equal (remove 3 '(1 2 3 4 3)) '(1 2 4 3)))
(define-test "remove-item-present-symbols"
  (assert-equal (remove 'b '(a b c b d)) '(a c b d)))
(define-test "remove-first-item"
  (assert-equal (remove 'x '(x y z)) '(y z)))
(define-test "remove-last-item"
  (assert-equal (remove 'z '(x y z)) '(x y)))
(define-test "remove-item-not-present"
  (assert-equal (remove 5 '(1 2 3 4)) '(1 2 3 4)))
(define-test "remove-from-single-element-list-match"
  (assert-equal (remove 'a '(a)) '()))
(define-test "remove-from-single-element-list-no-match"
  (assert-equal (remove 'b '(a)) '(a)))
(define-test "remove-list-from-list-of-lists"
  (assert-equal (remove '(2 3) '( (1 2) (2 3) (4 5) (2 3) )) '((1 2) (4 5) (2 3))))
(define-test "remove-non-destructive"
  (let ((original-list '(10 20 30 20 40)))
    (remove 20 original-list) ; Call remove
    (assert-equal original-list '(10 20 30 20 40)))) ; Original should be unchanged
