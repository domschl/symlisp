;;;-----------------------------------------------------------------------------
;;; Infix Expression Parser (Shunting-Yard Algorithm to Prefix S-expression)
;;;-----------------------------------------------------------------------------
;;; This module converts a list of infix tokens (strings) produced by
;;; string->infix-tokens into a standard SymLisp prefix S-expression.
;;;-----------------------------------------------------------------------------

;; Define operator properties (precedence and associativity)
;; Higher precedence binds tighter.
;; Associativity: 'left' means a op b op c -> ((a op b) op c)
;;                'right' means a op b op c -> (a op (b op c))
(define *original-operator-properties* ; Renamed to avoid conflict with extended one
  '(("+" (precedence 1) (associativity left))
    ("-" (precedence 1) (associativity left))
    ("*" (precedence 2) (associativity left))
    ("/" (precedence 2) (associativity left))
    ("^" (precedence 3) (associativity right))
    ;; Unary minus will be added as "@unary-"
    ))

;; Helper to get precedence of an operator string (uses local *operator-properties*)
(define (get-precedence op-str operator-properties)
  (let ((props (assoc op-str operator-properties)))
    (if props
        (cadr (assoc 'precedence (cdr props)))
        0)))

;; Helper to get associativity of an operator string (uses local *operator-properties*)
(define (get-associativity op-str operator-properties)
  (let ((props (assoc op-str operator-properties)))
    (if props
        (cadr (assoc 'associativity (cdr props)))
        'left)))

;; Helper to convert a token string to a number or symbol
;; Assumes string->number and string->symbol are available.
(define (string->atom token-str)
  (let ((num (string->number token-str))) ; string->number returns #f on failure
    (if num
        num ; It's a number
        (string->symbol token-str)))) ; Otherwise, treat as a symbol

;; Main parsing function using Shunting-Yard
;; Takes a list of string tokens.
;; Returns a prefix S-expression or signals an error.
(define (infix-tokens->prefix-expr raw-tokens)
  ;; Define extended operator properties including unary minus
  (define *operator-properties*
    (cons '("@unary-" (precedence 4) (associativity right)) ; Higher precedence for unary minus
          *original-operator-properties*))

  ;; Preprocessing step for unary minus: changes "-" to "@unary-" where appropriate
  (define (preprocess-unary-minus tokens)
    (let loop ((remaining tokens) (prev-token-type 'start) (acc '()))
      ;; prev-token-type can be 'start, 'operand, 'operator, 'lparen, 'rparen, 'comma
      (if (null? remaining)
          (reverse acc)
          (let ((current (car remaining)) (rest (cdr remaining)))
            (if (string=? current "-")
                (if (or (eq? prev-token-type 'start)
                        (eq? prev-token-type 'lparen)
                        (eq? prev-token-type 'operator)
                        (eq? prev-token-type 'comma))
                    (loop rest 'operator (cons "@unary-" acc)) ; It's unary
                    (loop rest 'operator (cons current acc)))  ; It's binary
                (let ((current-type
                       (cond ((assoc current *operator-properties*) 'operator)
                             ((string=? current "(") 'lparen)
                             ((string=? current ")") 'rparen)
                             ((string=? current ",") 'comma)
                             (else 'operand))))
                  (loop rest current-type (cons current acc))))))))

  ;; Helper to apply an operator from the stack to the operand stack
  ;; Returns (values new-operator-stack new-operand-stack)
  (define (apply-top-operator op-stack val-stack)
    (let* ((op-item (car op-stack))
           (rest-op-stack (cdr op-stack)))
      (cond
        ;; Unary operator "@unary-"
        ((string=? op-item "@unary-")
         (if (null? val-stack) (error "infix->prefix: Operand stack empty for unary operator" op-item))
         (let ((operand1 (car val-stack))
               (rest-val-stack (cdr val-stack)))
           (values rest-op-stack
                   (cons (list (string->symbol "-") operand1) rest-val-stack)))) ; Output as (- operand)
        
        ;; Function call (op-item is the function name string)
        ;; This is identified if op-item is not in *operator-properties*
        ((not (assoc op-item *operator-properties*))
         (if (or (null? rest-op-stack)
                 (not (list? (car rest-op-stack)))
                 (not (eq? (caar rest-op-stack) 'func-call-paren)))
             (error "infix->prefix: Expected function call marker not found for" op-item))
         (let* ((func-name-str op-item)
                (paren-marker (car rest-op-stack))
                (arg-count (cadr paren-marker))
                (final-rest-op-stack (cdr rest-op-stack)) ; Pop the paren-marker too
                (args '())
                (current-val-stack val-stack))
           (do ((i 0 (+ i 1))) ((= i arg-count))
             (if (null? current-val-stack) (error "infix->prefix: Not enough operands on stack for function" func-name-str arg-count (length val-stack)))
             (set! args (cons (car current-val-stack) args)) ; Collect in correct order
             (set! current-val-stack (cdr current-val-stack)))
           (values final-rest-op-stack
                   (cons (cons (string->symbol func-name-str) args) current-val-stack))))
        
        ;; Binary operator (e.g. "+", "-", "*", "/", "^")
        (else
         (if (or (null? val-stack) (null? (cdr val-stack))) (error "infix->prefix: Operand stack needs two items for binary operator" op-item))
         (let* ((operand2 (car val-stack))
                (operand1 (cadr val-stack))
                (rest-val-stack (cddr val-stack))
                (op-sym (string->symbol op-item)))
           (values rest-op-stack
                   (cons (list op-sym operand1 operand2) rest-val-stack)))))))

  (let ((tokens (preprocess-unary-minus raw-tokens)))
    (let loop ((toks tokens) (op-s '()) (val-s '()) (last-token-made-value? #f))
      (if (null? toks)
          ;; --- Base Case: No more tokens ---
          (let final-apply ((current-op-s op-s) (current-val-s val-s))
            (if (null? current-op-s)
                (if (and (pair? current-val-s) (null? (cdr current-val-s)))
                    (car current-val-s)
                    (error "infix->prefix: Invalid expression - operand stack malformed at end" current-val-s))
                (begin
                  (let ((top-op (car current-op-s)))
                    (if (or (string=? top-op "(")
                            (and (list? top-op) (eq? (car top-op) 'func-call-paren)))
                        (error "infix->prefix: Mismatched parentheses at end" top-op)))
                  (call-with-values (lambda () (apply-top-operator current-op-s current-val-s))
                    (lambda (next-op-s next-val-s)
                      (final-apply next-op-s next-val-s))))))
          ;; --- Recursive Step: Process next token ---
          (let ((token (car toks)) (rest-toks (cdr toks)))
            (cond
              ;; Case 1: Identifier (variable or function name) or Number
              ((and (string? token) ; Assuming numbers are also strings from tokenizer
                    (not (assoc token *operator-properties*))
                    (not (string=? token "("))
                    (not (string=? token ")"))
                    (not (string=? token ",")))
               (if (and (pair? rest-toks) (string=? (car rest-toks) "("))
                   ;; It's a function name, push to operator stack.
                   (loop rest-toks (cons token op-s) val-s #f)
                   ;; It's a variable/operand.
                   (loop rest-toks op-s (cons (string->atom token) val-s) #t)))

              ;; Case 2: Operator (e.g. "+", "-", "*", "/", "^", "@unary-")
              ((assoc token *operator-properties*)
               (let 지속적으로-적용 ((current-op-s op-s) (current-val-s val-s)) ; apply-continuously
                 (if (and (pair? current-op-s)
                          (let ((top-op (car current-op-s)))
                            (and (string? top-op) ; Make sure it's an operator string
                                 (assoc top-op *operator-properties*) ; And it's a defined operator
                                 (let ((top-prec (get-precedence top-op *operator-properties*))
                                       (current-prec (get-precedence token *operator-properties*)))
                                   (or (> top-prec current-prec)
                                       (and (= top-prec current-prec) 
                                            (eq? (get-associativity token *operator-properties*) 'left)))))))
                     (call-with-values (lambda () (apply-top-operator current-op-s current-val-s))
                       (lambda (next-op-s next-val-s)
                         (지속적으로-적용 next-op-s next-val-s)))
                     ;; Done applying higher precedence ops, now push current token.
                     (loop rest-toks (cons token current-op-s) current-val-s #f))))

              ;; Case 3: Opening Parenthesis "("
              ((string=? token "(")
               (if (and (pair? op-s) (string? (car op-s)) (not (assoc (car op-s) *operator-properties*)))
                   ;; Top of op-stack is a function name string. This ( is for its arguments.
                   ;; Push 'func-call-paren marker with initial arg count 0.
                   (loop rest-toks (cons (list 'func-call-paren 0) op-s) val-s #f)
                   ;; Regular (
                   (loop rest-toks (cons token op-s) val-s #f)))

              ;; Case 4: Closing Parenthesis ")"
              ((string=? token ")")
               (let pop-until-paren ((current-op-s op-s) (current-val-s val-s) (processed-value-for-arg? last-token-made-value?))
                 (cond
                   ((null? current-op-s) (error "infix->prefix: Mismatched parentheses - unexpected ')'"))
                   ;; Found opening paren for a function call
                   ((and (list? (car current-op-s)) (eq? (caar current-op-s) 'func-call-paren)) ; Top of op-stack is (func-call-paren K)
                    (let* ((paren-marker-on-top (car current-op-s)) ; This is (func-call-paren K_original)
                           (op-stack-below-marker (cdr current-op-s))) ; This should be ("func-name" ...rest...)
                      (if (or (null? op-stack-below-marker) 
                              (not (string? (car op-stack-below-marker)))
                              (assoc (car op-stack-below-marker) *operator-properties*)) ; Ensure it's a func name, not an operator
                          (error "infix->prefix: Expected function name below func-call-paren marker on stack" 
                                 paren-marker-on-top op-stack-below-marker))
                      (let* ((func-name-str (car op-stack-below-marker))
                             (actual-rest-op-s (cdr op-stack-below-marker)) ; Stack below function name
                             (original-arg-count (cadr paren-marker-on-top))
                             ;; If a value was processed right before ')', it's the last (or only) argument.
                             ;; If no value was processed (e.g. f()), arg count doesn't increment here.
                             (final-arg-count (if processed-value-for-arg?
                                                  (+ original-arg-count 1)
                                                  original-arg-count))
                             (updated-paren-marker-for-apply (list 'func-call-paren final-arg-count))
                             ;; Construct the stack for apply-top-operator: 
                             ;; ("func-name" (func-call-paren final-count) ...actual_rest_op_s...)
                             (stack-to-pass-to-apply (cons func-name-str 
                                                           (cons updated-paren-marker-for-apply actual-rest-op-s))))
                        (call-with-values (lambda () (apply-top-operator stack-to-pass-to-apply current-val-s))
                          (lambda (resulting-op-s resulting-val-s)
                            ;; resulting-op-s will be actual_rest_op_s after func name and marker are popped by apply-top-operator
                            (loop rest-toks resulting-op-s resulting-val-s #t))))))
                   ;; Found regular opening paren
                   ((string=? (car current-op-s) "(")
                    (loop rest-toks (cdr current-op-s) current-val-s #t)) ; Pop "(", discard ")"
                   ;; Operator inside parens
                   (else
                    (call-with-values (lambda () (apply-top-operator current-op-s current-val-s))
                      (lambda (next-op-s next-val-s)
                        (pop-until-paren next-op-s next-val-s #t)))))))

              ;; Case 5: Comma "," (Function argument separator)
              ((string=? token ",")
               (if (not last-token-made-value?) (error "infix->prefix: Comma not preceded by an argument value."))
               (let pop-until-func-paren ((current-op-s op-s) (current-val-s val-s))
                 (cond
                   ((null? current-op-s) (error "infix->prefix: Comma outside function or mismatched parens"))
                   ;; Found the 'func-call-paren' marker for the current function
                   ((and (list? (car current-op-s)) (eq? (caar current-op-s) 'func-call-paren))
                    (let* ((paren-marker (car current-op-s))
                           (arg-count (cadr paren-marker))
                           ;; Increment arg count in the marker on stack because an arg before comma is now complete.
                           (new-op-s (cons (list 'func-call-paren (+ arg-count 1)) (cdr current-op-s))))
                      (loop rest-toks new-op-s current-val-s #f))) ; #f because comma itself isn't a value
                   ((string=? (car current-op-s) "(") 
                    (error "infix->prefix: Comma encountered inside regular parentheses"))
                   ;; Operator before comma (part of current argument)
                   (else
                    (call-with-values (lambda () (apply-top-operator current-op-s current-val-s))
                      (lambda (next-op-s next-val-s)
                        (pop-until-func-paren next-op-s next-val-s)))))))
              (else (error "infix->prefix: Unrecognized token or invalid expression structure near" token toks))))))))

;; Convenience function: string -> prefix expression
(define (string->prefix-expr str)
  (let ((tokens (string->infix-tokens str))) ; Assuming string->infix-tokens exists
    (if (not tokens) 
        (error "string->prefix-expr: Tokenization failed for" str)
        (infix-tokens->prefix-expr tokens)))) ; Corrected closing parenthesis

;; --- Examples ---
;; (string->prefix-expr "1 + 2")        ; Expected: (+ 1 2)
;; (string->prefix-expr "a * b")        ; Expected: (* a b)
;; (string->prefix-expr "1 + 2 * 3")    ; Expected: (+ 1 (* 2 3))
;; (string->prefix-expr "(1 + 2) * 3")  ; Expected: (* (+ 1 2) 3)
;; (string->prefix-expr "a ^ b ^ c")    ; Expected: (^ a (^ b c)) (right-associative)
;; (string->prefix-expr "3 - 4 + 5")    ; Expected: (+ (- 3 4) 5) (left-associative)
;; (string->prefix-expr "-1 + 2")       ; Expected: (+ (- 1) 2)
;; (string->prefix-expr "f(x) + g(x,y)") ; Expected: (+ (f x) (g x y))
;; (string->prefix-expr "f()")          ; Expected: (f)
;; (string->prefix-expr "f(a,b*c,d)")   ; Expected: (f a (* b c) d)
;; (string->prefix-expr "-a * (b + -c)") ; Expected: (* (- a) (+ b (- c)))