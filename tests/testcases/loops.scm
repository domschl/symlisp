;; --- DO ---

(define-test-thunked "do-simple-count"
  (lambda ()
    (assert-equal
      (do ((i 0 (+ i 1))
           (sum 0 (+ sum i)))
          ((>= i 5) sum))
      10)))

(define-test-thunked "do-with-commands"
  (lambda ()
    (let ((x 0))
      (assert-equal
        (do ((i 0 (+ i 1)))
            ((= i 3) x) ; Stop when i=3, return x
          (set! x (+ x i))) ; Command executed before step
        3)))) ; i=0, x=0; i=1, x=1; i=2, x=3; stop

(define-test-thunked "do-no-commands"
  (lambda ()
    (assert-equal
      (do ((i 0 (+ i 1)))
          ((= i 4) i)) ; Stop when i=4, return i
      4)))

(define-test-thunked "do-multiple-vars"
  (lambda ()
    (assert-equal
      (do ((i 0 (+ i 1))
           (j 10 (- j 1)))
          ((= i j) (list i j))) ; Stop when i equals j
      '(5 5)))) ; i: 0 1 2 3 4 5 / j: 10 9 8 7 6 5

(define-test-thunked "do-no-result-expr" ; Should return unspecified (NIL)
  (lambda ()
    (assert-equal
      (do ((i 0 (+ i 1)))
          ((= i 1))) ; Stop when i=1, no result expr
      '()))) ; Expecting NIL for unspecified

(define-test-thunked "do-multiple-result-exprs"
  (lambda ()
    (assert-equal
      (do ((i 0 (+ i 1)))
          ((= i 2) (display "done") (+ i 10))) ; Returns value of last result expr
      12)))

(define-test-thunked "do-no-bindings" ; Not standard, but test behavior if allowed
  (lambda ()
    (assert-equal
      (do ()
          (#t 42)) ; Test is immediately true
      42)))

(define-test-thunked "do-no-step"
  (lambda ()
    (assert-equal
      (do ((i 5 i)) ; Step is just 'i' (no change)
          ((= i 5) i))
      5)))
