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
(define *operator-properties*
  '(("+" (precedence 1) (associativity left))
    ("-" (precedence 1) (associativity left))
    ("*" (precedence 2) (associativity left))
    ("/" (precedence 2) (associativity left))
    ("^" (precedence 3) (associativity right))
    ;; TODO: Add unary minus/plus, function calls
    ))

;; Helper to get precedence of an operator string
(define (get-precedence op-str)
  (let ((props (assoc op-str *operator-properties*)))
    (if props
        (cadr (assoc 'precedence (cdr props)))
        ;; Default precedence for non-operators or '('
        ;; '(' needs low precedence when on stack, high when incoming?
        ;; Let's treat '(' as having 0 precedence on the stack for now.
        0)))

;; Helper to get associativity of an operator string
(define (get-associativity op-str)
  (let ((props (assoc op-str *operator-properties*)))
    (if props
        (cadr (assoc 'associativity (cdr props)))
        'left))) ; Default associativity (doesn't matter much for non-ops)

;; Helper to convert a token string to a number or symbol
;; Assumes string->number and string->symbol are available.
(define (string->atom token-str)
  (let ((num (string->number token-str))) ; string->number returns #f on failure
    (if num
        num ; It's a number
        (string->symbol token-str)))) ; Otherwise, treat as a symbol

;; Helper to apply an operator from the stack to the operand stack
;; Returns a new operand stack with the result pushed.
;; Assumes operator-stack is non-empty.
;; TODO: Handle unary operators (pop 1 operand).
;; TODO: Handle function calls (pop N operands).
(define (apply-top-operator operator-stack operand-stack)
  (let* ((op-str (car operator-stack))
         (op-sym (string->symbol op-str)) ; Convert operator string to symbol
         ;; For binary operators:
         (operand2 (car operand-stack))
         (operand1 (cadr operand-stack))
         (new-expr (list op-sym operand1 operand2)))
    ;; Return the new operand stack
    (cons new-expr (cddr operand-stack))))

;; Main parsing function using Shunting-Yard
;; Takes a list of string tokens.
;; Returns a prefix S-expression or signals an error.
(define (infix-tokens->prefix-expr tokens)
  (let loop ((remaining-tokens tokens)
             (operator-stack '()) ; Stack for operators and '('
             (operand-stack '())) ; Stack for operands and intermediate results
    (cond
      ;; --- Base Case: No more tokens ---
      ((null? remaining-tokens)
       (cond
         ;; Operator stack is empty, result should be the single item on operand stack
         ((null? operator-stack)
          (if (and (pair? operand-stack) (null? (cdr operand-stack)))
              (car operand-stack)
              (error "infix->prefix: Invalid expression - operand stack malformed at end" operand-stack)))
         ;; Still operators on stack, apply them
         ((string=? (car operator-stack) "(")
          (error "infix->prefix: Mismatched parentheses - unclosed '(' at end"))
         (else
          ;; Apply the top operator
          (let ((new-operand-stack (apply-top-operator operator-stack operand-stack)))
            ;; Recurse with empty tokens, popped operator stack, new operand stack
            (loop '() (cdr operator-stack) new-operand-stack)))))

      ;; --- Recursive Step: Process next token ---
      (else
       (let ((token (car remaining-tokens))
             (rest-tokens (cdr remaining-tokens)))
         (cond
           ;; Case 1: Number or Identifier (convert and push to operand stack)
           ((and (string? token)
                 (not (assoc token *operator-properties*)) ; Not an operator
                 (not (string=? token "("))
                 (not (string=? token ")"))
                 (not (string=? token ","))) ; Assuming ',' is only for functions
            (loop rest-tokens operator-stack (cons (string->atom token) operand-stack)))

           ;; Case 2: Operator
           ((assoc token *operator-properties*)
            (let ((current-prec (get-precedence token))
                  (current-assoc (get-associativity token)))
              ;; While stack not empty, top is not '(', and top has higher/equal precedence...
              (if (and (pair? operator-stack)
                       (not (string=? (car operator-stack) "("))
                       (let ((top-prec (get-precedence (car operator-stack))))
                         (or (> top-prec current-prec)
                             (and (= top-prec current-prec) (eq? current-assoc 'left)))))
                  ;; Apply the top operator first
                  (let ((new-operand-stack (apply-top-operator operator-stack operand-stack)))
                    ;; Retry processing the *same* token with updated stacks
                    (loop remaining-tokens (cdr operator-stack) new-operand-stack))
                  ;; Otherwise, push the current operator onto the stack
                  (loop rest-tokens (cons token operator-stack) operand-stack))))

           ;; Case 3: Opening Parenthesis
           ((string=? token "(")
            ;; Push onto operator stack
            (loop rest-tokens (cons token operator-stack) operand-stack))

           ;; Case 4: Closing Parenthesis
           ((string=? token ")")
            (cond
              ((null? operator-stack)
               (error "infix->prefix: Mismatched parentheses - unexpected ')'"))
              ((string=? (car operator-stack) "(")
               ;; Found matching '(', pop it from op stack, discard ')' token
               (loop rest-tokens (cdr operator-stack) operand-stack))
              (else
               ;; Apply operator inside parens
               (let ((new-operand-stack (apply-top-operator operator-stack operand-stack)))
                 ;; Retry processing the *same* ')' token with updated stacks
                 (loop remaining-tokens (cdr operator-stack) new-operand-stack)))))

           ;; Case 5: Comma (Function argument separator) - TODO
           ((string=? token ",")
            ;; Pop operators until '(' is found. Don't pop '('.
            (error "infix->prefix: Function calls not yet implemented"))

           ;; Default: Error
           (else (error "infix->prefix: Unrecognized token" token))))))))

;; Convenience function: string -> prefix expression
(define (string->prefix-expr str)
  (infix-tokens->prefix-expr (string->infix-tokens str)))

;; --- Examples ---
;; (string->prefix-expr "1 + 2")        ; Expected: (+ 1 2)
;; (string->prefix-expr "a * b")        ; Expected: (* a b)
;; (string->prefix-expr "1 + 2 * 3")    ; Expected: (+ 1 (* 2 3))
;; (string->prefix-expr "(1 + 2) * 3")  ; Expected: (* (+ 1 2) 3)
;; (string->prefix-expr "a ^ b ^ c")    ; Expected: (^ a (^ b c)) (right-associative)
;; (string->prefix-expr "3 - 4 + 5")    ; Expected: (+ (- 3 4) 5) (left-associative)
