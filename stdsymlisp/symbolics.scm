;;;-----------------------------------------------------------------------------
;;; SymLisp Symbolic Algebra System
;;; Phase 1: Core Expression Representation and Basic Simplification Infrastructure
;;;-----------------------------------------------------------------------------

;;; --- 1.1: Expression Structure & Predicates ---

;; An expression is either:
;; 1. A number (constant)
;; 2. A symbol (variable or operator name)
;; 3. A list representing a compound expression, e.g., (operator operand1 operand2 ...)

;; Helper: List of known operators. This might grow.
;; For now, it helps distinguish variables from operators if they are symbols.
(define *known-operators* '(+ - * / ^ sin cos tan log exp sqrt)) ; Add more as needed

;; Predicates

;; (constant? expr) -> #t if expr is a number, #f otherwise
(define (constant? expr)
  (number? expr))

;; (variable? expr) -> #t if expr is a symbol and not a known operator, #f otherwise
(define (variable? expr)
  (and (symbol? expr)
       (not (memq expr *known-operators*)))) ; memq checks for presence in the list

;; (atomic-expr? expr) -> #t if expr is a constant or a variable
(define (atomic-expr? expr)
  (or (constant? expr)
      (variable? expr)))

;; (compound-expr? expr) -> #t if expr is a list and its car is an operator
(define (compound-expr? expr)
  (and (pair? expr) ; Must be a non-empty list
       (symbol? (car expr))
       (if (memq (car expr) *known-operators*) #t #f))) ; Ensure #t is returned if memq succeeds

;; Type-specific predicates for compound expressions

;; (sum? expr) -> #t if expr is a sum, e.g., (+ a b ...)
(define (sum? expr)
  (and (compound-expr? expr)
       (eq? (car expr) '+)))

;; (product? expr) -> #t if expr is a product, e.g., (* a b ...)
(define (product? expr)
  (and (compound-expr? expr)
       (eq? (car expr) '*)))

;; (power? expr) -> #t if expr is a power, e.g., (^ base exponent)
(define (power? expr)
  (and (compound-expr? expr)
       (eq? (car expr) '^)
       (pair? (cdr expr))      ; Must have at least one operand (base)
       (pair? (cddr expr))     ; Must have two operands (base, exponent)
       (null? (cddr (cdr expr))) ; Must have exactly two operands
       ))

;; (negation? expr) -> #t if expr is a negation, e.g., (- a)
;; Note: This assumes unary minus is represented as (- operand)
;; If you adopt (* -1 operand), this predicate might change or be less used.
(define (negation? expr)
  (and (compound-expr? expr)
       (eq? (car expr) '-)
       (pair? (cdr expr))      ; Must have one operand
       (null? (cddr expr))))   ; Must have exactly one operand

;; (difference? expr) -> #t if expr is a difference, e.g., (- a b)
(define (difference? expr)
  (and (compound-expr? expr)
       (eq? (car expr) '-)
       (pair? (cdr expr))      ; Must have at least one operand
       (pair? (cddr expr))     ; Must have two operands
       (null? (cddr (cdr expr))) ; Must have exactly two operands
       ))

;; (quotient? expr) -> #t if expr is a quotient, e.g., (/ a b)
(define (quotient? expr)
  (and (compound-expr? expr)
       (eq? (car expr) '/)
       (pair? (cdr expr))      ; Must have at least one operand (numerator)
       (pair? (cddr expr))     ; Must have two operands (numerator, denominator)
       (null? (cddr (cdr expr))) ; Must have exactly two operands
       ))

;; Add more specific predicates for other functions (sin, cos, etc.) as needed
;; e.g., (sine? expr), (cosine? expr)

;; Accessors

;; (operator expr) -> returns the operator symbol from a compound expression
;; Precondition: (compound-expr? expr) is true
(define (operator expr)
  (if (compound-expr? expr)
      (car expr)
      (error "operator: Not a compound expression" expr)))

;; (operands expr) -> returns the list of operands from a compound expression
;; Precondition: (compound-expr? expr) is true
(define (operands expr)
  (if (compound-expr? expr)
      (cdr expr)
      (error "operands: Not a compound expression" expr)))

;; Accessors for sums (assuming n-ary for flexibility)
;; (terms expr) -> returns the list of terms in a sum
;; Precondition: (sum? expr) is true
(define (terms expr)
  (if (sum? expr)
      (operands expr)
      (error "terms: Not a sum expression" expr)))

;; (addend1 expr) -> first term in a binary sum
;; (define (addend1 expr) (cadr expr)) ; if strictly binary
;; (addend2 expr) -> second term in a binary sum
;; (define (addend2 expr) (caddr expr)) ; if strictly binary

;; Accessors for products (assuming n-ary for flexibility)
;; (factors expr) -> returns the list of factors in a product
;; Precondition: (product? expr) is true
(define (factors expr)
  (if (product? expr)
      (operands expr)
      (error "factors: Not a product expression" expr)))

;; (multiplicand1 expr) -> first factor in a binary product
;; (multiplicand2 expr) -> second factor in a binary product

;; Accessors for powers
;; (base expr) -> returns the base of a power expression
;; Precondition: (power? expr) is true
(define (base expr)
  (if (power? expr)
      (cadr expr) ; First operand
      (error "base: Not a power expression" expr)))

;; (exponent expr) -> returns the exponent of a power expression
;; Precondition: (power? expr) is true
(define (exponent expr)
  (if (power? expr)
      (caddr expr) ; Second operand
      (error "exponent: Not a power expression" expr)))

;; Accessors for unary minus/negation
;; (negated-expr expr) -> returns the expression being negated
;; Precondition: (negation? expr) is true
(define (negated-expr expr)
  (if (negation? expr)
      (cadr expr)
      (error "negated-expr: Not a negation" expr)))

;; Accessors for difference
;; (minuend expr) -> returns the minuend of a difference expression
;; Precondition: (difference? expr) is true
(define (minuend expr)
  (if (difference? expr)
      (cadr expr)
      (error "minuend: Not a difference expression" expr)))

;; (subtrahend expr) -> returns the subtrahend of a difference expression
;; Precondition: (difference? expr) is true
(define (subtrahend expr)
  (if (difference? expr)
      (caddr expr)
      (error "subtrahend: Not a difference expression" expr)))

;; Accessors for quotients
;; (quotient-numerator expr) -> returns the numerator of a quotient expression
;; Precondition: (quotient? expr) is true
(define (quotient-numerator expr)
  (if (quotient? expr)
      (cadr expr)
      (error "quotient-numerator: Not a quotient expression" expr)))

;; (quotient-denominator expr) -> returns the denominator of a quotient expression
;; Precondition: (quotient? expr) is true
(define (quotient-denominator expr)
  (if (quotient? expr)
      (caddr expr)
      (error "quotient-denominator: Not a quotient expression" expr)))
