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
    (assert-equal (filter odd? '(1 2 3 4 5)) '(1 3 5)))) ; Assuming odd? predicate