;;; Tests for Higher-Order Functions

;; --- MAP ---

(define-test-thunked "map-single-list"
  (lambda ()
    (assert-equal (map (lambda (x) (* x x)) '(1 2 3 4)) '(1 4 9 16))))

(define-test-thunked "map-two-lists"
  (lambda ()
    (assert-equal (map + '(1 2 3) '(4 5 6)) '(5 7 9))))

(define-test-thunked "map-three-lists"
  (lambda ()
    (assert-equal (map (lambda (a b c) (* a (+ b c))) '(1 2 3) '(4 5 6) '(7 8 9)) '(11 26 45))))

(define-test-thunked "map-different-lengths"
  (lambda ()
    (assert-equal (map list '(1 2) '(a b c)) '((1 a) (2 b))))) ; Stops when shortest list ends

(define-test-thunked "map-empty-list"
  (lambda ()
    (assert-equal (map + '() '()) '())
    (assert-equal (map + '(1 2) '()) '())))

;(define-test-thunked "map-error-proc-arity"
;  (lambda ()
;    (assert-error (map (lambda (x) x) '(1 2) '(3 4)) "Apply: Mismatched argument count"))) ; lambda expects 1, map provides 2

;(define-test-thunked "map-error-not-a-proc"
;  (lambda ()
;    (assert-error (map 1 '(2 3)) "map: First argument must be a procedure")))

;(define-test-thunked "map-error-not-a-list"
;  (lambda ()
;    (assert-error (map + '(1 2) 3) "map: All list arguments must be proper lists")))

;; --- FILTER ---

(define-test-thunked "filter-simple"
  (lambda ()
    (assert-equal (filter odd? '(1 2 3 4 5)) '(1 3 5)))) ; Assuming odd? predicate;; filepath: /Users/dsc/Codeberg/SymLisp/tests/testcases/higher_order.scm

(define-test-thunked "filter-empty-list"
  (lambda ()
    (assert-equal (filter odd? '()) '())))

(define-test-thunked "filter-all-pass"
  (lambda ()
    (assert-equal (filter number? '(1 2 3 4)) '(1 2 3 4)))) ; Assuming number? predicate

(define-test-thunked "filter-none-pass"
  (lambda ()
    (assert-equal (filter symbol? '(1 2 3 4)) '()))) ; Assuming symbol? predicate

(define-test-thunked "filter-mixed-types"
  (lambda ()
    (assert-equal (filter pair? '(a (b) c (d e) f)) '((b) (d e))))) ; Assuming pair? predicate

(define-test-thunked "filter-keep-order"
  (lambda ()
    (assert-equal (filter (lambda (x) (> x 3)) '(5 1 4 2 6 3)) '(5 4 6))))


;; --- FOR-EACH ---
(define-test-thunked "for-each-simple"
  (lambda ()
    (let ((sum 0))
      (for-each (lambda (x) (set! sum (+ sum x))) '(1 2 3 4))
      (assert-equal sum 10)))) ; Check side effect

(define-test-thunked "for-each-multiple-lists"
  (lambda ()
    (let ((pairs '()))
      (for-each (lambda (x y) (set! pairs (cons (list x y) pairs)))
                '(1 2 3)
                '(a b c))
      (assert-equal pairs '((3 c) (2 b) (1 a)))))) ; Note: cons reverses order

(define-test-thunked "for-each-empty"
  (lambda ()
    (let ((called #f))
      (for-each (lambda (x) (set! called #t)) '())
      (assert-equal called #f)))) ; Ensure proc not called for empty list

;; --- FOLD-LEFT / REDUCE ---
(define-test-thunked "fold-left-sum"
  (lambda ()
    (assert-equal (fold-left + 0 '(1 2 3 4)) 10)))

(define-test-thunked "fold-left-difference" ; ((1 - 2) - 3) - 4
  (lambda ()
    (assert-equal (fold-left - 0 '(1 2 3 4)) -10))) ; 0-1=-1, -1-2=-3, -3-3=-6, -6-4=-10

(define-test-thunked "fold-left-list"
  (lambda ()
    (assert-equal (fold-left cons '() '(1 2 3 4)) '((((() . 1) . 2) . 3) . 4)))) ; Left association

(define-test-thunked "fold-left-multiple-lists"
  (lambda ()
    (assert-equal (fold-left (lambda (acc x y) (cons* x y acc)) '() '(1 2 3) '(a b c))
                  '(3 c 2 b 1 a )))) ; (cons* 1 a (cons* 2 b (cons* 3 c '()))) -> (1 a 2 b 3 c) reversed

(define-test-thunked "fold-left-no-lists"
  (lambda ()
    (assert-equal (fold-left + 100) 100))) ; Returns initial value

(define-test-thunked "reduce-alias" ; Test reduce alias for fold-left
  (lambda ()
    (assert-equal (reduce + 0 '(1 2 3 4)) 10)))

;; --- FOLD-RIGHT ---
(define-test-thunked "fold-right-sum"
  (lambda ()
    (assert-equal (fold-right + 0 '(1 2 3 4)) 10)))

(define-test-thunked "fold-right-difference" ; 1 - (2 - (3 - (4 - 0)))
  (lambda ()
    (assert-equal (fold-right - 0 '(1 2 3 4)) -2))) ; 4-0=4, 3-4=-1, 2-(-1)=3, 1-3=-2

(define-test-thunked "fold-right-list"
  (lambda ()
    (assert-equal (fold-right cons '() '(1 2 3 4)) '(1 2 3 4)))) ; Right association (builds list)

(define-test-thunked "fold-right-multiple-lists"
  (lambda ()
    (assert-equal (fold-right (lambda (x y acc) (cons* x y acc)) '() '(1 2 3) '(a b c))
                  '(1 a 2 b 3 c )))) ; (cons* 1 a (cons* 2 b (cons* 3 c '())))

(define-test-thunked "fold-right-no-lists"
  (lambda ()
    (assert-equal (fold-right + 100) 100))) ; Returns initial value
