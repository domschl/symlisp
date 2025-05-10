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

;; =====================================================================
(define (get-infix-op-properties op-symbol arity)
  (cond
    ((eq? op-symbol '+) (list "+" 1 'left)) ; Precedence 1, left-associative
    ((eq? op-symbol '-)
     (if (= arity 1) (list "-" 4 'right)      ; Unary minus (precedence 4, higher than binary)
         (list "-" 1 'left)))                 ; Binary minus (precedence 1, left-associative)
    ((eq? op-symbol '*) (list "*" 2 'left)) ; Precedence 2, left-associative
    ((eq? op-symbol '/) (if (= arity 2) (list "/" 2 'left) #f)) ; Precedence 2, left, strictly binary for now
    ((eq? op-symbol '^) (if (= arity 2) (list "^" 3 'right) #f)) ; Precedence 3, right, strictly binary for now
    (else #f)))

(define (needs-parentheses-for-display current-prec current-is-left-assoc parent-prec parent-is-left-assoc am-i-left-child-of-parent)
  (if (= parent-prec 0) ; Parent is top-level or function call context (precedence 0 indicates no specific operator parent)
      #f
      (cond
        ((< current-prec parent-prec) #t) ; Current op has lower precedence than parent, needs parens.
        ((> current-prec parent-prec) #f) ; Current op has higher precedence, no parens.
        (else ;; Equal precedence
         (if parent-is-left-assoc
             (not am-i-left-child-of-parent) ; Parent L-assoc, current is R-child: needs parens (e.g., a-(b-c))
             am-i-left-child-of-parent)))))  ; Parent R-assoc, current is L-child: needs parens (e.g., (a^b)^c)

;; Helper to join a list of strings with a delimiter (if not already available)
(define (string-join lst delim)
  (cond ((null? lst) "")
        ((null? (cdr lst)) (if (string? (car lst)) (car lst) (error "string-join: non-string element" (car lst))))
        (else
         (let ((item1 (car lst)))
           (if (not (string? item1)) (error "string-join: non-string element" item1))
           (string-append item1 delim (string-join (cdr lst) delim))))))

;; Main function to convert prefix S-expression to infix string
(define (prefix-expr->string expr)
  (expr->infix-string-recursive expr #f 0 #f #f))

(define (expr->infix-string-recursive expr parent-op-symbol parent-prec parent-is-left-assoc am-i-left-child-of-parent)
  (cond
    ((number? expr) (number->string expr))
    ((symbol? expr) (symbol->string expr))
    ((not (pair? expr)) (error "expr->infix-string: Invalid expression structure" expr))
    ;; Add this new condition for sqrt BEFORE the general list/operator handling
    ((and (power? expr) (equal? (exponent expr) 1/2)) ; Check for (^ base 1/2)
     (string-append "sqrt(" (expr->infix-string-recursive (base expr) #f 0 #f #f) ")"))
    (else ; It's a list (op arg1 ...)
     (let* ((op (car expr)) (args (cdr expr)) (num-args (length args)))
       (if (and (memq op '(+ *)) (= num-args 1))
           (expr->infix-string-recursive (car args) parent-op-symbol parent-prec parent-is-left-assoc am-i-left-child-of-parent)
           (let ((op-properties (get-infix-op-properties op num-args)))
             (cond
               (op-properties ; Known infix operator
                (let* ((op-char (car op-properties))
                       (current-prec (cadr op-properties))
                       (current-assoc (caddr op-properties))
                       (current-is-left-assoc (eq? current-assoc 'left))
                       (infix-str ""))
                  (cond
                    ;; Unary minus (distinct properties from binary minus)
                    ((and (eq? op '-) (= num-args 1))
                     (let ((operand-string (expr->infix-string-recursive 
                                             (car args) 
                                             op ; Parent op for recursive call is current op (unary '-')
                                             current-prec 
                                             current-is-left-assoc 
                                             #f))) ; Operand is not a "left" child
                       ;; If operand string itself starts with a minus (e.g. is "-k" or "-(a+b)"),
                       ;; then wrap it in parens to avoid "--k" or "--(a+b)".
                       (if (and (> (string-length operand-string) 0)
                                (char=? (string-ref operand-string 0) #\-))
                           (set! infix-str (string-append "-" "(" operand-string ")")) 
                           (set! infix-str (string-append "-" operand-string)))))
                    
                    ;; Strictly Binary operators: / and ^
                    ((and (memq op '(/ ^)) (= num-args 2))
                     (set! infix-str 
                           (string-append (expr->infix-string-recursive (car args) op current-prec current-is-left-assoc #t)
                                          " " op-char " "
                                          (expr->infix-string-recursive (cadr args) op current-prec current-is-left-assoc #f))))
                    
                    ;; Operators that can be variadic or binary: +, *, and binary -
                    ((memq op '(+ * -))
                     (cond
                       ((= num-args 0) ; e.g. (+) or (*) - should be simplified by Scheme eval
                        (set! infix-str (if (eq? op '+) "0" "1"))) ; Default representation
                       ((and (eq? op '-) (= num-args 1)) ; (- x) if unary minus properties weren't distinct (should be caught above)
                        (set! infix-str ; Fallback for (- x) if it reached here
                              (string-append op-char
                                             (expr->infix-string-recursive (car args) op current-prec current-is-left-assoc #f))))
                       (else ; num-args >= 2 for +, *, and binary - (or num-args = 1 for +/* handled by outer if)
                        (let ((first-arg-str (expr->infix-string-recursive (car args) op current-prec current-is-left-assoc #t))
                              (rest-arg-strings (map (lambda (arg)
                                                       (expr->infix-string-recursive arg op current-prec current-is-left-assoc #f))
                                                     (cdr args))))
                          (if (null? rest-arg-strings) ; Should only be true if num-args was 1 and not filtered
                              (set! infix-str first-arg-str)
                              (set! infix-str (string-append first-arg-str
                                                             (apply string-append ; Join rest with " op "
                                                                    (map (lambda (s) (string-append " " op-char " " s))
                                                                         rest-arg-strings)))))))))
                    (else 
                     (error "expr->infix-string: Unhandled arity or form for known operator properties" op)))
                  
                  (if (needs-parentheses-for-display current-prec current-is-left-assoc parent-prec parent-is-left-assoc am-i-left-child-of-parent)
                      (string-append "(" infix-str ")")
                      infix-str)))
               
               ((symbol? op) ; Generic function call for other symbols
                (let ((func-name (symbol->string op))
                      (arg-strings (map (lambda (arg) (expr->infix-string-recursive arg #f 0 #f #f)) args)))
                  (string-append func-name "(" (string-join arg-strings ", ") ")")))
               (else (error "expr->infix-string: Unhandled expression form" expr)))))))))

;; Helper to map function symbols to LaTeX command strings
(define (latex-function-name-map sym-str)
  (cond ((string=? sym-str "sin") "\\sin")
        ((string=? sym-str "cos") "\\cos")
        ((string=? sym-str "tan") "\\tan")
        ((string=? sym-str "log") "\\log") ; Typically base 10 or context-dependent
        ((string=? sym-str "ln") "\\ln")   ; Natural log
        ((string=? sym-str "sqrt") "\\sqrt") ; sqrt is often special
        ;; Add other common functions as needed
        ((> (string-length sym-str) 1) (string-append "\\operatorname{" sym-str "}")) ; For user-defined multi-char
        (else sym-str))) ; Single char like f, g, x, y

;; Main function to convert prefix S-expression to Markdown with LaTeX
(define (prefix-expr->markdown-latex expr)
  (string-append "$" (expr->latex-recursive expr #f 0 #f #f) "$"))

;; Recursive helper for prefix-expr->markdown-latex
(define (expr->latex-recursive expr parent-op-symbol parent-prec parent-is-left-assoc am-i-left-child-of-parent)
  (cond
    ((number? expr) (number->string expr))
    ((symbol? expr)
     (let ((s (symbol->string expr)))
       (cond ((string=? s "pi") "\\pi") 
             (else (latex-function-name-map s))))) 
    ((not (pair? expr)) (error "expr->latex: Invalid expression structure" expr))
    ;; Add this new condition for sqrt BEFORE the general list/operator handling
    ((and (power? expr) (equal? (exponent expr) 1/2)) ; Check for (^ base 1/2)
     (string-append "\\sqrt{" (expr->latex-recursive (base expr) #f 0 #f #f) "}"))
    (else ; It's a list (op arg1 ...)
     (let* ((op (car expr)) (args (cdr expr)) (num-args (length args)))
         (if (and (memq op '(+ *)) (= num-args 1))
           (expr->latex-recursive (car args) parent-op-symbol parent-prec parent-is-left-assoc am-i-left-child-of-parent)
           (let ((op-properties (get-infix-op-properties op num-args)))
             (cond
               (op-properties ; Known infix operator
                (let* ((op-char-for-display (car op-properties)) 
                       (current-prec (cadr op-properties))
                       (current-assoc (caddr op-properties))
                       (current-is-left-assoc (eq? current-assoc 'left))
                       (latex-str ""))
                  (cond
                    ;; Unary Minus (- x)
                    ((and (eq? op '-) (= num-args 1))
                     (let ((operand-latex (expr->latex-recursive 
                                            (car args) 
                                            op 
                                            current-prec 
                                            current-is-left-assoc 
                                            #f)))
                       ;; If operand-latex itself starts with a minus, wrap it in parens for LaTeX.
                       (if (and (> (string-length operand-latex) 0)
                                (char=? (string-ref operand-latex 0) #\-)) ; Assuming char=? is available or use string=? with substring
                           (set! latex-str (string-append "-\\left(" operand-latex "\\right)")) 
                           (set! latex-str (string-append "-" operand-latex)))))
                    
                    ;; Division (/ num den)
                    ((and (eq? op '/) (= num-args 2))
                     (set! latex-str
                           (string-append "\\frac{"
                                          (expr->latex-recursive (car args) #f 0 #f #f) 
                                          "}{"
                                          (expr->latex-recursive (cadr args) #f 0 #f #f) 
                                          "}")))
                    ;; Power (^ base exp)
                    ((and (eq? op '^) (= num-args 2))
                     (let* ((base-expr (car args))
                            (exponent-expr (cadr args))
                            (base-latex "")
                            (exp-latex (expr->latex-recursive exponent-expr #f 0 #f #f))) ; Exp context reset
                       ;; Special formatting for functions like sin^2(x)
                       (if (and (pair? base-expr) (symbol? (car base-expr))
                                (memq (car base-expr) '(sin cos tan log ln))) ; Add more if needed
                           (let ((func-name-latex (latex-function-name-map (symbol->string (car base-expr))))
                                 (func-args-expr (cdr base-expr)))
                             (set! base-latex
                                   (string-append func-name-latex
                                                  "^{" exp-latex "}"
                                                  "\\left("
                                                  (string-join (map (lambda (arg) (expr->latex-recursive arg #f 0 #f #f)) func-args-expr) ", ")
                                                  "\\right)")))
                           ;; Standard base^{exp}
                           (set! base-latex
                                 (string-append (expr->latex-recursive base-expr op current-prec current-is-left-assoc #t)
                                                "^{" exp-latex "}")))
                       (set! latex-str base-latex)))
                    
                    ;; Operators that can be variadic or binary: +, *, and binary -
                    ((memq op '(+ * -))
                     (let ((latex-op-str (cond ((eq? op '+) " + ")
                                               ((eq? op '-) " - ") ; Binary minus
                                               ((eq? op '*) " \\cdot ")
                                               (else ""))))
                       (cond
                         ((= num-args 0)
                          (set! latex-str (if (eq? op '+) "0" "1")))
                         ((and (eq? op '-) (= num-args 1)) ; Should have been caught by unary minus rule above
                          (set! latex-str ; Fallback
                                (string-append "-" (expr->latex-recursive (car args) op current-prec current-is-left-assoc #f))))
                         (else ; num-args >= 2 for +, *, binary - (or num-args = 1 for +/* handled by outer if)
                          (let ((first-arg-latex (expr->latex-recursive (car args) op current-prec current-is-left-assoc #t))
                                (rest-args-latex (map (lambda (arg)
                                                        (expr->latex-recursive arg op current-prec current-is-left-assoc #f))
                                                      (cdr args))))
                            (if (null? rest-args-latex)
                                (set! latex-str first-arg-latex)
                                (set! latex-str (string-append first-arg-latex
                                                               (apply string-append
                                                                      (map (lambda (s) (string-append latex-op-str s))
                                                                           rest-args-latex))))))))))
                    (else
                     (error "expr->latex: Unhandled arity or form for known operator properties" op)))
                  
                  ;; Add LaTeX parentheses if needed, skip for \frac and some ^ cases.
                  (if (and (not (eq? op '/)) 
                           (not (and (eq? op '^) 
                                     (pair? (car args)) (symbol? (caar args))
                                     (memq (caar args) '(sin cos tan log ln)))))
                      (if (needs-parentheses-for-display current-prec current-is-left-assoc parent-prec parent-is-left-assoc am-i-left-child-of-parent)
                          (string-append "\\left(" latex-str "\\right)")
                          latex-str)
                      latex-str)))
               
               ((symbol? op) 
                (let* ((func-name-str (symbol->string op))
                       (latex-op (latex-function-name-map func-name-str))
                       (arg-strings (map (lambda (arg) (expr->latex-recursive arg #f 0 #f #f)) args)))
                  (if (and (string=? latex-op "\\sqrt") (= num-args 1)) 
                      (string-append latex-op "{" (car arg-strings) "}")
                      (string-append latex-op "\\left(" (string-join arg-strings ", ") "\\right)"))))
               (else (error "expr->latex: Unhandled expression form" expr)))))))))