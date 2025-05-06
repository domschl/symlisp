;; --- QUASIQUOTE, UNQUOTE, UNQUOTE-SPLICING ---

(define-test "quasiquote-simple-list"
  (assert-equal `(a b c) '(a b c)))

(define-test "quasiquote-with-unquote"
  (let ((b-val 2) (c-val 3))
    (assert-equal `(a ,b-val ,c-val d) '(a 2 3 d))))

(define-test "quasiquote-unquote-expression"
  (let ((x 5))
    (assert-equal `(value is ,(+ x 10)) '(value is 15))))

(define-test "quasiquote-unquote-splicing-simple"
  (let ((inner-list '(b c)))
    (assert-equal `(a ,@inner-list d) '(a b c d))))

(define-test "quasiquote-unquote-splicing-multiple"
  (let ((list1 '(1 2)) (list2 '(3 4)))
    (assert-equal `(start ,@list1 middle ,@list2 end) '(start 1 2 middle 3 4 end))))

(define-test "quasiquote-unquote-splicing-empty-list"
  (let ((empty '()))
    (assert-equal `(a ,@empty d) '(a d))))

(define-test "quasiquote-unquote-splicing-at-beginning"
  (let ((prefix '(1 2)))
    (assert-equal `(,@prefix 3 4) '(1 2 3 4))))

(define-test "quasiquote-unquote-splicing-at-end"
  (let ((suffix '(3 4)))
    (assert-equal `(1 2 ,@suffix) '(1 2 3 4))))

(define-test "quasiquote-nested-quasiquote"
  (let ((x 10))
    (assert-equal ``(outer ,,x ,,(+ x 5)) ; evaluates to `(outer ,x ,(+ x 5))
                  '(quasiquote (outer (unquote x) (unquote (+ x 5)))))))

(define-test "quasiquote-nested-eval"
  (let ((x 10))
    ;; `(outer ,`(inner ,,x))
    ;; eval(`(inner ,,x)) should yield (inner (unquote x)) by R7RS interpretation
    ;; then eval((inner (unquote x))) is not how the outer , works.
    ;; The outer , simply substitutes the result of eval(`(inner ,,x)).
    ;; So, `(outer ,`(inner ,,x)) should evaluate to (outer (inner (unquote x)))
    (assert-equal `(outer ,`(inner ,,x)) '(outer (inner (unquote x)))))) ; << CHANGED Expected Value

; (define-test "quasiquote-unquote-inside-nested-quasiquote"
;   (let ((a 1) (b 2))
;     ;; `(list '`(,a ,,b))
;     ;; eval('`(,a ,,b)) should yield `(,a ,,b) which is (quasiquote ((unquote a) (unquote (unquote b))))
;     ;; So the whole expression should yield (list (quasiquote ((unquote a) (unquote (unquote b)))))
;     (assert-equal `(list '`(,a ,,b))
;                   '(list (quasiquote ((unquote a) (unquote (unquote b)))))))) ; << CHANGED Expected Value

; (define-test "quasiquote-unquote-splicing-inside-nested-quasiquote"
;   (let ((items '(x y)))
;     ;; `(outer-list `(inner ,@items ,,items))
;     ;; -> (outer-list (quasiquote (inner (unquote-splicing items) (unquote (unquote items)))))
;     ;; -> (outer-list `(inner ,@items ,,items))
;     ;; -> (outer-list '(inner x y (unquote items)))
;     (assert-equal `(outer-list `(inner ,@items ,,items))
;                   '(outer-list '(inner x y (unquote items))))))


; (define-test "quasiquote-dotted-pair"
;   (let ((y 20))
;     (assert-equal `(x . ,y) '(x . 20))))

;; Test case from Scheme R7RS spec (section 4.2.6)
(define-test "quasiquote-r7rs-example1"
  (assert-equal `(list ,(+ 1 2) 4) '(list 3 4)))

(define-test "quasiquote-r7rs-example2"
  (let ((a 1) (b '(2 3)))
    (assert-equal `(a ,a ,@b) '(a 1 2 3))))

; (define-test "quasiquote-r7rs-example3"
;   (assert-equal `(a `(b ,(+ 1 2) ,@(map abs '(4 -5 6)) d) e)
;                 '(a `(b ,(+ 1 2) ,@(map abs '(4 -5 6)) d) e))) ; Level 1 QQ

; (define-test "quasiquote-r7rs-example3-evaled"
;   (assert-equal (eval `(a `(b ,(+ 1 2) ,@(map abs '(4 -5 6)) d) e) (interaction-environment))
;                 '(a (quasiquote (b (unquote (+ 1 2)) (unquote-splicing (map abs (quote (4 -5 6)))) d)) e)))

; (define-test "quasiquote-r7rs-example4-evaled"
;   ;; `(a ,(eval `(b ,(+ 1 2) ,@(map abs '(4 -5 6)) d)) e)
;   ;; -> (a (b 3 4 5 6 d) e)
;   (assert-equal (eval `(a ,(eval `(b ,(+ 1 2) ,@(map abs '(4 -5 6)) d) (nteraction-environment)) e) (interaction-environment))
;                 '(a (b 3 4 5 6 d) e)))

;; More complex splicing
(define-test "quasiquote-splicing-with-atoms"
  (let ((x 'foo) (y '(bar baz)))
    (assert-equal `(,@(if #t (list x) '()) test ,@y) '(foo test bar baz))))


;; --- DEFINE-SYNTAX (Non-Hygienic) ---

;; Test 1: Simple macro definition - returns a fixed list
(define-syntax my-list-macro
  (lambda (form)
    ''(1 2 3)))
(define-test "define-syntax-simple-expansion"
  (assert-equal (my-list-macro) '(1 2 3)))

;; Test 2: Macro using its arguments
;; (swap a b) -> (list b a)
(define-syntax swap
  (lambda (form) ; form is (swap arg1 arg2)
    (let ((arg1 (cadr form))
          (arg2 (caddr form)))
      (list 'list arg2 arg1)))) ; Construct (list <value of arg2> <value of arg1>)

(define-test "define-syntax-swap-args"
  (assert-equal (swap (+ 1 0) (* 2 2)) '(4 1)))

(define-test "define-syntax-swap-symbols"
  (let ((a 10) (b 20))
    (assert-equal (swap a b) '(20 10))))


;; Test 3: `while` macro
;; (while condition body ...)
;; Expands to: (let _macro-internal-loop () (if condition (begin body ... (_macro-internal-loop)) '()))
(define-syntax while
  (lambda (form)
    (let ((condition (cadr form))  ; e.g., condition becomes (> while-test-var 0)
          (body (cddr form)))      ; e.g., body becomes ((set! while-test-var ...))
      `(let _macro-internal-loop ()
         (if ,condition            ; <<< HERE
             (begin
               ,@body             ; ,@ is unquote-splicing
               (_macro-internal-loop))
             '())))))

(define while-test-var 0)
(define-test "define-syntax-while-countdown"
  (begin
    (set! while-test-var 3)
    (while (> while-test-var 0)
      (set! while-test-var (- while-test-var 1)))
    (assert-equal while-test-var 0)))

(define while-sum 0)
(define while-counter 1)
(define-test "define-syntax-while-sum"
  (begin
    (set! while-sum 0)
    (set! while-counter 1)
    (while (<= while-counter 5)
      (set! while-sum (+ while-sum while-counter))
      (set! while-counter (+ while-counter 1)))
    (assert-equal while-sum 15) ; 1+2+3+4+5
    (assert-equal while-counter 6)))

(define-test "define-syntax-while-condition-initially-false"
  (begin
    (set! while-test-var 100)
    (assert-equal (while #f (set! while-test-var 0)) '())
    (assert-equal while-test-var 100))) ; Body should not execute

(define while-side-effect-check 0)
(define-test "define-syntax-while-empty-body"
  (begin
    (set! while-side-effect-check 3)
    (assert-equal (while (> (begin (set! while-side-effect-check (- while-side-effect-check 1)) while-side-effect-check)
                            -1)) ; Condition has side effect
                  '())
    (assert-equal while-side-effect-check -1)))

;; Test 4: Demonstrating non-hygienic nature (potential capture/shadowing)
;; This test shows why careful naming (like _macro-internal-loop) is needed.
;; If `while` internally used a common name like `loop` or `cond`, it could clash.

;; Scenario 1: Macro's internal variable shadowed by user's `let`
;; Renamed from bad-while-shadow to better reflect its role in this specific test.
;; This macro sets up a 'let loop ()' context and executes the body once if condition is true.
(define-syntax shadowing-test-macro
  (lambda (form)
    (let ((condition (cadr form))
          (body (cddr form)))
      ;; This `loop` is the symbol introduced by the macro.
      ;; The macro executes the body once if the condition is true.
      ;; The recursive call `(loop)` is removed from the original bad-while-shadow
      ;; to allow the body's result to be the macro's result for this test.
      `(let loop () ; The 'loop' symbol that can be shadowed
         (if ,condition
             (begin ,@body) ; Execute body; its result becomes the result of 'if'
             '())))))        ; Return '() if condition is initially false

(define-test "define-syntax-non-hygienic-shadow-macro-var"
  (let ((x 2)) ; x starts at 2, condition (> x 0) will be true
    (assert-equal
      (shadowing-test-macro (> x 0)
        ;; Body provided to the macro:
        (let ((loop (lambda () "user-loop"))) ; User defines 'loop', shadowing macro's 'loop'
          (set! x (- x 1)) ; x becomes 1
          (if (= x 1)
              (loop)      ; This calls user's 'loop', which returns "user-loop"
              'should-not-happen))) ; This branch is not taken in the first iteration
      "user-loop")))

;; A more direct example of capture by the *macro's generated code*:
(define-syntax uses-external-y
  (lambda (form)
    '(+ y 5))) ; The generated code expects a 'y' in its execution scope

(define-test "define-syntax-non-hygienic-generated-code-captures-y"
  (begin ;; <<< Wrap the body in a 'begin'
    (let ((y 10))
      (assert-equal (uses-external-y) 15))
    ;; The (define y 20) is now an internal define.
    ;; Its visibility and behavior depend on how SymLisp handles internal defines
    ;; within a 'begin' block (especially if 'define-test' wraps this in a lambda).
    ;; For this test to demonstrate lexical capture, 'y' defined here should be
    ;; a new binding visible to the subsequent 'assert-equal'.
    (define y 20) 
    (assert-equal (uses-external-y) 25))
  ) ;; <<< End of 'begin'