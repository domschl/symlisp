;;; Tests for eval, environment, interaction-environment

;; --- environment? ---
(define-test "environment?-true-interaction" (lambda () (assert-equal (environment? (interaction-environment)) #t)))
(define-test "environment?-true-new" (lambda () (assert-equal (environment? (environment)) #t)))
(define-test "environment?-false-num" (lambda () (assert-equal (environment? 123) #f)))
(define-test "environment?-false-str" (lambda () (assert-equal (environment? "hello") #f)))
(define-test "environment?-false-bool" (lambda () (assert-equal (environment? #t) #f)))
(define-test "environment?-false-sym" (lambda () (assert-equal (environment? 'symbol) #f)))
(define-test "environment?-false-list" (lambda () (assert-equal (environment? '(1 2)) #f)))
(define-test "environment?-false-proc" (lambda () (assert-equal (environment? (lambda (x) x)) #f)))
(define-test "environment?-false-char" (lambda () (assert-equal (environment? #\a) #f)))
(define-test "environment?-false-nil" (lambda () (assert-equal (environment? '()) #f)))

;; --- eval ---

;; Basic evaluation in interaction-environment
(define-test "eval-number" (lambda () (assert-equal (eval 123 (interaction-environment)) 123)))
(define-test "eval-string" (lambda () (assert-equal (eval '"str" (interaction-environment)) "str"))) ; Note: eval takes expression, not value
(define-test "eval-boolean-t" (lambda () (assert-equal (eval #t (interaction-environment)) #t)))
(define-test "eval-boolean-f" (lambda () (assert-equal (eval #f (interaction-environment)) #f)))
(define-test "eval-nil" (lambda () (assert-equal (eval ''() (interaction-environment)) '())))
(define-test "eval-quoted-symbol" (lambda () (assert-equal (eval ''a (interaction-environment)) 'a)))
(define-test "eval-simple-expr" (lambda () (assert-equal (eval '(+ 1 2) (interaction-environment)) 3)))
(define-test "eval-list-construction" (lambda () (assert-equal (eval '(list 1 2) (interaction-environment)) '(1 2))))

;; Arity and Type Errors
;(define-test "eval-arity-error-0"
;  (lambda () (assert-error (eval) "eval: Expected 2 arguments, got 0.")))
;(define-test "eval-arity-error-1"
;  (lambda () (assert-error (eval '(+ 1 1)) "eval: Expected 2 arguments, got 1.")))
;(define-test "eval-arity-error-3"
;  (lambda () (assert-error (eval '(+ 1 1) (interaction-environment) 'extra) "eval: Expected 2 arguments, got 3.")))
;(define-test "eval-env-type-error-num"
;  (lambda () (assert-error (eval '(+ 1 1) 5) "eval: Second argument must be an environment, got number.")))
;(define-test "eval-env-type-error-list"
;  (lambda () (assert-error (eval '(+ 1 1) '(1 2)) "eval: Second argument must be an environment, got pair."))

;; Evaluation errors within the evaluated expression
;(define-test "eval-inner-unbound"
;  (lambda () (assert-error (eval 'undefined-variable (interaction-environment)) "Unbound variable: undefined-variable.")))
;(define-test "eval-inner-type-error"
;  (lambda () (assert-error (eval '(+ 1 'a) (interaction-environment)) "+: Expected number, got symbol: 'a'."))

;; Using interaction-environment
(define global-x-for-eval 50)
(define-test "eval-read-global"
  (lambda () (assert-equal (eval 'global-x-for-eval (interaction-environment)) 50)))

(define-test "eval-define-global"
  (lambda ()
    (begin
      (eval '(define global-y-for-eval 100) (interaction-environment))
      (assert-equal global-y-for-eval 100))))


;; Nested eval
(define-test "eval-nested-quote"
  (lambda () (assert-equal (eval '(eval ''inner-quote (interaction-environment)) (interaction-environment)) 'inner-quote)))
(define-test "eval-nested-expr"
  (lambda () (assert-equal (eval '(eval '(+ 10 20) (interaction-environment)) (interaction-environment)) 30)))

;; Using custom environments
;(define-test "eval-in-new-env-cant-see-global"
;  (lambda () (assert-error (eval 'global-x-for-eval (environment)) "Unbound variable: global-x-for-eval.")))

(define-test "eval-define-in-new-env"
  (lambda ()
    (let ((new-env (environment)))
      (eval '(define local-var 77) new-env)
      (assert-equal (eval 'local-var new-env) 77))))


(define-test "eval-lambda-in-new-env"
  (lambda ()
    (let ((new-env (environment)))
      ;; Define 'add' in the new environment. It closes over the global '+'
      (eval '(define add (lambda (a b) (+ a b))) new-env)
      ;; Evaluate the call '(add 3 5)' within the new environment
      (assert-equal (eval '(add 3 5) new-env) 8))))


(define-test "eval-lambda-capture-new-env"
  (lambda ()
    (let ((new-env (environment)))
      ;; Define 'local-val' and 'adder' in the new environment
      (eval '(define local-val 10) new-env)
      (eval '(define adder (lambda (x) (+ x local-val))) new-env)
      ;; Retrieve 'adder' procedure from the new environment
      (let ((proc (eval 'adder new-env)))
        ;; Call the procedure in the current (interaction) environment.
        ;; It should still use 'local-val' from the environment it was defined in.
        (assert-equal (proc 5) 15)))))


(define-test "eval-lambda-capture-new-env-modified"
  (lambda ()
    (let ((new-env (environment)))
      (eval '(define local-val-2 10) new-env)
      (eval '(define adder-2 (lambda (x) (+ x local-val-2))) new-env)
      (let ((proc (eval 'adder-2 new-env)))
        (assert-equal (proc 5) 15)
        ;; Modify 'local-val-2' in the new environment *after* creating the lambda
        (eval '(set! local-val-2 100) new-env)
        ;; Call the procedure again. It should see the updated value.
        (assert-equal (proc 5) 105)))))


;(define-test "eval-cant-see-let-vars"
;  (let ((let-var 99))
;    (assert-error (eval 'let-var (interaction-environment)) "Unbound variable: let-var.")))

(define-test "eval-can-see-global-funcs-from-child"
  (lambda () (assert-equal (eval '(list 1 2 3) (environment)) '(1 2 3))))