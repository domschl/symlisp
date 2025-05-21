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
(define *known-operators* '(+ - * / ^ abs sin cos tan ln exp sqrt expt)) ; Add more as needed

;; Predicates

;; (constant? expr) -> #t if expr is a number, #f otherwise
(define (constant? expr)
  (number? expr))

;; (rational? expr) -> #t if expr is a rational number.
;; In SymLisp, all numbers are currently rationals.
(define (rational? expr)
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

;; Helper for unary function predicates
(define (unary-op-check expr op-symbol)
  (and (compound-expr? expr)
       (eq? (car expr) op-symbol)
       (pair? (cdr expr))      ; Must have one operand
       (null? (cddr expr))))   ; Must have exactly one operand

;; Predicates for new special functions
(define (abs? expr) (unary-op-check expr 'abs))
(define (ln? expr) (unary-op-check expr 'ln))
(define (exp? expr) (unary-op-check expr 'exp))
(define (sin? expr) (unary-op-check expr 'sin))
(define (cos? expr) (unary-op-check expr 'cos))
(define (tan? expr) (unary-op-check expr 'tan))
(define (sqrt? expr) (unary-op-check expr 'sqrt))

;; Predicate for (expt base exponent)
(define (expt? expr)
  (and (compound-expr? expr)
       (eq? (car expr) 'expt)
       (pair? (cdr expr))          ; Must have base
       (pair? (cddr expr))         ; Must have exponent
       (null? (cddr (cdr expr))))) ; Must have exactly two operands

;; Accessors for (expt base exponent)
(define (expt-base expr)
  (if (expt? expr)
      (cadr expr)
      (error "expt-base: Not an expt expression" expr)))

(define (expt-exponent expr)
  (if (expt? expr)
      (caddr expr)
      (error "expt-exponent: Not an expt expression" expr)))

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

;; --- Rule Engine Core ---

(define *simplify-rules* '())
;; Define other rule sets as needed later
;; (define *expand-rules* '())

;; Helper to check if a symbol is a pattern variable (e.g., ?x)
(define (pattern-variable? sym)
  (and (symbol? sym)
       (let ((s (symbol->string sym)))
         (and (> (string-length s) 0)
              (char=? #\? (string-ref s 0))))))

;; (get-binding bindings var-symbol) -> value or #f
;; Retrieves the value bound to a pattern variable from the bindings alist.
(define (get-binding bindings var-symbol)
  (let ((pair (assoc var-symbol bindings)))
    (if pair
        (cdr pair)
        #f)))

;; (match pattern expr bindings) -> new-bindings or #f
;; Tries to match expr against pattern, extending bindings.
(define (match pattern expr bindings)
  (cond
    ((eq? pattern '_) bindings) ; Wildcard matches anything, no new binding
    ((pattern-variable? pattern)
     (let ((found-binding (assoc pattern bindings)))
       (if found-binding
           (if (equal? (cdr found-binding) expr)
               bindings ; Variable already bound to the same value
               #f)      ; Variable bound to a different value, match fails
           (cons (cons pattern expr) bindings)))) ; New binding
    ((and (atom? pattern) (atom? expr)) ; Literals (numbers, non-pattern symbols)
     (if (equal? pattern expr) bindings #f))
    ((and (pair? pattern) (pair? expr))
     (let ((car-bindings (match (car pattern) (car expr) bindings)))
       (if car-bindings
           (match (cdr pattern) (cdr expr) car-bindings)
           #f)))
    ((and (null? pattern) (null? expr)) bindings) ; Both empty lists match
    (else #f))) ; Structure mismatch or type mismatch

;; (transform template bindings) -> new-expression
;; Constructs a new expression from template using bindings.
(define (transform template bindings)
  (cond
    ((null? template) '())
    ((pattern-variable? template)
     (let ((binding (get-binding bindings template)))
       (if binding
           binding
           (error "transform: Unbound pattern variable in template" template))))
    ((atom? template) template) ; Literal
    ((pair? template)
     (cons (transform (car template) bindings)
           (transform (cdr template) bindings)))
    (else (error "transform: Unknown template structure" template))))

;; Helper to find the index of an item in a list (needed by apply-rules)
;; Uses 'equal?' for comparison by default.
(define (list-position item lst . cmp)
  (let ((compare (if (null? cmp) equal? (car cmp))))
    (let loop ((sub-list lst) (idx 0))
      (cond
        ((null? sub-list) #f)
        ((compare (car sub-list) item) idx)
        (else (loop (cdr sub-list) (+ idx 1)))))))

;; (apply-rules expr ruleset) -> (cons new-expr #t) if a rule applied,
;;                            or (cons expr #f) if no rule applied.
(define (apply-rules expr ruleset)
  (let loop ((remaining-rules ruleset))
    (if (null? remaining-rules)
        (cons expr #f) ; No rule applied
        (let* ((rule (car remaining-rules))
               (rule-id (car rule)) ; For debugging
               (operation (cadr rule)) ; For context
               (pattern-idx 2)
               (pattern (car (list-drop rule pattern-idx)))
               (arrow-idx (list-position '=> rule)))

          (if (not arrow-idx)
              (begin
                (display "Error: Malformed rule, missing '=>': ") (display rule) (newline)
                (error "apply-rules: Malformed rule structure"))
              #f) ; Dummy value, proceed if arrow-idx is found

          (let* ((template (car (list-drop rule (+ arrow-idx 1))))
                 (condition-proc #f) ; Renamed from 'condition'
                 (condition-candidate-idx (+ pattern-idx 1)))

            (if (< condition-candidate-idx arrow-idx) ; A condition predicate exists
                (set! condition-proc (car (list-drop rule condition-candidate-idx))))

            (let ((bindings (match pattern expr '())))
              (if bindings
                  (if condition-proc ; Check if a condition procedure exists
                      (if (condition-proc bindings) ; Call the procedure stored in condition-proc
                          (cons (transform template bindings) #t)
                          (loop (cdr remaining-rules))) ; Condition false, try next rule
                      (cons (transform template bindings) #t)) ; No condition, rule applies
                  (loop (cdr remaining-rules))))))))) ; Match failed, try next rule

;; --- End Rule Engine Core ---

;; --- Initial Set of Simplification Rules ---
(set! *simplify-rules*
  (list
   ;; Rules from simplify-exp
   (list 'simplify-exp-zero 'simplify '(exp 0) '=> 1)
   (list 'simplify-exp-one 'simplify '(exp 1) '=> 'e)
   (list 'simplify-exp-ln 'simplify '(exp (ln ?x)) '=> '?x)
   (list 'simplify-exp-prod-k-ln-x 'simplify '(exp (* ?k (ln ?x))) '=> '(^ ?x ?k)) ; Corrected template

   ;; Rules from simplify-ln
   (list 'simplify-ln-one 'simplify '(ln 1) '=> 0)
   (list 'simplify-ln-e 'simplify '(ln e) '=> 1)
   (list 'simplify-ln-exp 'simplify '(ln (exp ?x)) '=> '?x)
   (list 'simplify-ln-pow 'simplify '(ln (^ ?base ?exponent)) '=> '(* ?exponent (ln ?base))) ; Corrected template

   ;; Rules from simplify-quotient
   (list 'simplify-quotient-num-zero 'simplify '(/ 0 ?den)
         (lambda (b) (not (equal? (get-binding b '?den) 0))) ; Condition: den != 0
         '=> 0)
   (list 'simplify-quotient-den-one 'simplify '(/ ?num 1) '=> '?num)
   (list 'simplify-quotient-self 'simplify '(/ ?x ?x)
         (lambda (b) (not (equal? (get-binding b '?x) 0))) ; Condition: x != 0
         '=> 1)
   (list 'simplify-tan-def-from-quotient 'simplify '(/ (sin ?arg) (cos ?arg)) '=> '(tan ?arg))

   ;; --- Rules from simplify-power ---
   ;; (^ 0 ?e) -> 0, if e > 0
   (list 'simplify-pow-base-zero 'simplify '(^ 0 ?e)
         (lambda (b) (let ((e-val (get-binding b '?e))) (and (number? e-val) (> e-val 0))))
         '=> 0)
   ;; (^ 1 ?e) -> 1
   (list 'simplify-pow-base-one 'simplify '(^ 1 ?e) '=> 1)
   ;; (^ ?b 0) -> 1, if b != 0
   (list 'simplify-pow-exp-zero 'simplify '(^ ?b 0)
         (lambda (b) (not (equal? (get-binding b '?b) 0)))
         '=> 1)
   ;; (^ ?b 1) -> ?b
   (list 'simplify-pow-exp-one 'simplify '(^ ?b 1) '=> '?b)

   ;; (^ i int-exp) ; four rules for i^n mod 4
   (list 'simplify-pow-i-mod4-0 'simplify '(^ i ?e)
         (lambda (b) (let ((e-val (get-binding b '?e))) (and (integer? e-val) (= (modulo e-val 4) 0))))
         '=> 1)
   (list 'simplify-pow-i-mod4-1 'simplify '(^ i ?e)
         (lambda (b) (let ((e-val (get-binding b '?e))) (and (integer? e-val) (= (modulo e-val 4) 1))))
         '=> 'i)
   (list 'simplify-pow-i-mod4-2 'simplify '(^ i ?e)
         (lambda (b) (let ((e-val (get-binding b '?e))) (and (integer? e-val) (= (modulo e-val 4) 2))))
         '=> -1)
   (list 'simplify-pow-i-mod4-3 'simplify '(^ i ?e)
         (lambda (b) (let ((e-val (get-binding b '?e))) (and (integer? e-val) (= (modulo e-val 4) 3))))
         '=> '(- i))

   ;; (^ (num-const) (int-const)) -> (expt num-const int-const)
   (list 'simplify-pow-const-base-int-exp 'simplify '(^ ?cb ?ce)
         (lambda (b) (let ((cb-val (get-binding b '?cb)) (ce-val (get-binding b '?ce)))
                       (and (number? cb-val) (integer? ce-val)
                            ;; Avoid 0 to non-positive integer power for direct evaluation by this rule
                            (not (and (= cb-val 0) (<= ce-val 0))))))
         '=> '(expt ?cb ?ce)) ; Relies on simplify evaluating (expt num1 num2)

   ;; (^ (^ ?b1 ?e1) ?e2) -> (^ ?b1 (* ?e1 ?e2))
   ;; (list 'simplify-pow-of-pow 'simplify '(^ (^ ?b1 ?e1) ?e2) '=> '(^ ?b1 (* ?e1 ?e2)))

   ;; (^ (- ?base-neg) (?e :integer :even :non-negative)) -> (^ ?base-neg ?e)
   (list 'simplify-pow-neg-base-even-exp 'simplify '(^ (- ?bn) ?e)
         (lambda (b) (let ((e-val (get-binding b '?e)))
                       (and (integer? e-val) (even? e-val) (>= e-val 0))))
         '=> '(^ ?bn ?e))

   ;; (^ (- ?base-neg) (?e :integer :odd :non-negative)) -> (- (^ ?base-neg ?e))
   (list 'simplify-pow-neg-base-odd-exp 'simplify '(^ (- ?bn) ?e)
         (lambda (b) (let ((e-val (get-binding b '?e)))
                       (and (integer? e-val) (odd? e-val) (>= e-val 0))))
         '=> '(- (^ ?bn ?e)))
   ))


;; Generic accessor for unary function arguments
;; Precondition: The corresponding predicate (e.g., abs?) should be true.
(define (unary-arg expr)
  (cadr expr))

;; Accessors for new special functions
(define (abs-arg expr)
  (if (abs? expr) (unary-arg expr) (error "abs-arg: Not an abs expression" expr)))
(define (ln-arg expr)
  (if (ln? expr) (unary-arg expr) (error "ln-arg: Not an ln expression" expr)))
(define (exp-arg expr)
  (if (exp? expr) (unary-arg expr) (error "exp-arg: Not an exp expression" expr)))
(define (sin-arg expr)
  (if (sin? expr) (unary-arg expr) (error "sin-arg: Not a sin expression" expr)))
(define (cos-arg expr)
  (if (cos? expr) (unary-arg expr) (error "cos-arg: Not a cos expression" expr)))
(define (tan-arg expr)
  (if (tan? expr) (unary-arg expr) (error "tan-arg: Not a tan expression" expr)))
(define (sqrt-arg expr)
  (if (sqrt? expr) (unary-arg expr) (error "sqrt-arg: Not a sqrt expression" expr)))

;;; --- 1.2: Basic Simplify Function ---

;; Helper: Check if all expressions in a list are constants
(define (all-constants? exprs)
  (if (null? exprs)
      #t ; An empty list of expressions can be considered all constants
      (and (constant? (car exprs))
           (all-constants? (cdr exprs)))))

;; --- Helper for Canonical Ordering ---

;; (term<? t1 t2) -> #t if t1 should come before t2 in canonical order.
;; Order: constants (numeric) < variables (alphabetic) < compound expressions (string representation)
(define (term<? t1 t2)
  (cond
    ;; Both constants
    ((and (constant? t1) (constant? t2)) (< t1 t2))
    ;; t1 is constant, t2 is not
    ((constant? t1) #t)
    ;; t2 is constant, t1 is not
    ((constant? t2) #f)
    ;; Both variables
    ((and (variable? t1) (variable? t2)) (string<? (symbol->string t1) (symbol->string t2)))
    ;; t1 is variable, t2 is not (compound)
    ((variable? t1) #t)
    ;; t2 is variable, t1 is not (compound)
    ((variable? t2) #f)
    ;; Both compound expressions (or other types not yet specifically handled)
    ;; Fallback to string comparison for a stable, somewhat arbitrary order.
    ;; Assumes expr->string provides a consistent representation.
    (else (string<? (expr->string t1) (expr->string t2)))))

;; Helper: Construct a sum expression, simplifying if possible
;; (make-sum '(term1 term2 ...))
(define (make-sum terms)
  (cond
    ((null? terms) 0) ; Sum of no terms is 0
    ((null? (cdr terms)) (car terms)) ; Sum of one term is the term itself
    (else (cons '+ terms))))

;; Helper: Construct a product expression, simplifying if possible
;; (make-product '(factor1 factor2 ...))
(define (make-product factors)
  (cond
    ((null? factors) 1) ; Product of no factors is 1
    ((null? (cdr factors)) (car factors)) ; Product of one factor is the factor itself
    (else (cons '* factors))))

;; Helper: Construct a power expression, simplifying trivial exponent cases
(define (make-power base exponent)
  (cond
    ((and (number? exponent) (= exponent 0)) 1)
    ((and (number? exponent) (= exponent 1)) base)
    (else (list '^ base exponent))))

;; Helper to construct an expression
(define (make-abs arg)
  (list 'abs arg))

(define (make-exp arg)
  (list 'exp arg))

(define (make-ln arg)
  (list 'ln arg))

(define (make-sin arg) (list 'sin arg))
(define (make-cos arg) (list 'cos arg))
(define (make-tan arg) (list 'tan arg))

;; Forward declaration for simplify, as helpers might call it or be called by it.
(define simplify #f)

;; Helper: Extracts coefficient and base term from a term.
;; Returns a list: (coefficient base-term)
;; e.g., x -> (1 x)
;;       (* 2 x y) -> (2 (* x y))
;;       (* x y) -> (1 (* x y))
;;       5 -> (5 1) ; Constant term, base is 1 (neutral for multiplication)
(define (get-coefficient-and-base term)
  (cond
    ((constant? term) (list term 1)) ; Coefficient is term, base is 1
    ((variable? term) (list 1 term))  ; Coefficient 1, base is term
    ((product? term)
     (let ((ops (operands term)))
       (if (and (pair? ops) (constant? (car ops)))
           (list (car ops) (make-product (cdr ops))) ; Coeff is (car ops), base is rest
           (list 1 term))))                         ; No leading const coeff, coeff is 1, base is whole product
    (else (list 1 term)))) ; For other compound terms like (^ x 2), coeff is 1

(define (flatten-sum-terms-iterative terms-list)
  (let collect-flat ((input-terms terms-list) (output-terms '()))
    (if (null? input-terms)
        (reverse output-terms) ; Reverse to maintain relative order of original non-sum items
        (let ((term (car input-terms)))
          (if (sum? term)
              (collect-flat (append (operands term) (cdr input-terms)) output-terms)
              (collect-flat (cdr input-terms) (cons term output-terms)))))))

;; Extracts the numerical coefficient and the base variable part of a term.
;; Returns (values coefficient base-part)
;; Assumes 'term' is already simplified.
;; e.g., x -> (1 . x)
;;       (* 2 x) -> (2 . x)
;;       (* 2 x y) -> (2 . (* x y))
;;       (- x) -> (-1 . x)
;;       (^ x 2) -> (1 . (^ x 2))
;;       (* 2 (- x)) -> (-2 . x)
(define (get-coeff-base-term term)
  (cond
    ((constant? term) (values term 1)) ; Coefficient is term itself, base is a dummy '1' for constants
    ((variable? term) (values 1 term))
    ((negation? term)
     (call-with-values (lambda () (get-coeff-base-term (negated-expr term)))
       (lambda (coeff base)
         (values (* -1 coeff) base))))
    ((product? term)
     (let* ((factors (operands term))
            (num-val (apply * (filter constant? factors)))
            (non-const-factors (filter (lambda (f) (not (constant? f))) factors)))
       (cond
         ((null? non-const-factors) (values num-val 1)) ; Product was purely numeric
         ((null? (cdr non-const-factors)) (values num-val (car non-const-factors)))
         (else (values num-val (simplify (make-product non-const-factors))))))) ; Simplify the base part
    (else (values 1 term)))) ; Default for other complex terms like powers, etc.

;; Helper for simplify-sum: groups sorted like terms.
;; Assumes sorted-terms is a list of terms already sorted by term<?.
;; This sorting helps bring potentially like terms (e.g., x and (* c x)) closer,
;; but the primary grouping is by the "base part" of the term.
(define (collect-like-terms sorted-terms)
  (if (null? sorted-terms)
      '()
      (let* ((first-term-info (get-coefficient-and-base (car sorted-terms)))
             (current-coeff (car first-term-info))
             (current-base-term (cadr first-term-info)))
        (let process-terms ((remaining-terms (cdr sorted-terms))
                             (accumulated-coeff current-coeff)
                             (base-to-match current-base-term))
          (if (null? remaining-terms)
              ;; End of list, construct the final term for this group
              (list (simplify ; <<<< SIMPLIFY THE CONSTRUCTED TERM
                     (if (and (number? accumulated-coeff) (= accumulated-coeff 1) (not (equal? base-to-match 1)))
                         base-to-match
                         (if (equal? base-to-match 1)
                             accumulated-coeff
                             (make-product (list accumulated-coeff base-to-match))))))
              (let* ((next-term-info (get-coefficient-and-base (car remaining-terms)))
                     (next-coeff (car next-term-info))
                     (next-base-term (cadr next-term-info)))
                (if (equal? next-base-term base-to-match)
                    (process-terms (cdr remaining-terms)
                                   (add-coefficients accumulated-coeff next-coeff)
                                   base-to-match)
                    (cons (simplify ; <<<< SIMPLIFY THE CONSTRUCTED TERM
                           (if (and (number? accumulated-coeff) (= accumulated-coeff 1) (not (equal? base-to-match 1)))
                               base-to-match
                               (if (equal? base-to-match 1)
                                   accumulated-coeff
                                   (make-product (list accumulated-coeff base-to-match)))))
                          (collect-like-terms remaining-terms)))))))))

;; Helper to add coefficients (can be numbers or symbolic expressions)
(define (add-coefficients c1 c2)
  (simplify (make-sum (list c1 c2))))

(define (apply-sum-identity-rules terms-list)
  ;; This function would house the logic for applying rules specific to sums.
  ;; For now, let's imagine it just handles the Pythagorean identity.
  ;; It would return (values modified-terms-list new-constants-generated)
  (let search-pythagorean ((terms-to-scan terms-list)
                           (accumulated-non-identity-terms '())
                           (ones-added 0))
    (if (null? terms-to-scan)
        (values (reverse accumulated-non-identity-terms) ones-added)
        (let ((term1 (car terms-to-scan)))
          (cond
            ;; Case 1: term1 is sin^2(A)
            ((and (power? term1) (equal? (exponent term1) 2) (sin? (base term1)))
             (let ((argA (sin-arg (base term1))))
               (let find-cos-partner ((remaining (cdr terms-to-scan)) (skipped-terms '()))
                 (cond
                   ((null? remaining) ; No cos^2(A) partner found
                    (search-pythagorean (cdr terms-to-scan) (cons term1 accumulated-non-identity-terms) ones-added))
                   ((and (power? (car remaining)) (equal? (exponent (car remaining)) 2) (cos? (base (car remaining)))
                         (equal? (cos-arg (base (car remaining))) argA))
                    ;; Partner cos^2(A) found!
                    (search-pythagorean (append (reverse skipped-terms) (cdr remaining)) accumulated-non-identity-terms (+ ones-added 1)))
                   (else ; Not a partner, keep searching
                    (find-cos-partner (cdr remaining) (cons (car remaining) skipped-terms)))))))
            
            ;; Case 2: term1 is cos^2(A)
            ((and (power? term1) (equal? (exponent term1) 2) (cos? (base term1)))
             (let ((argA (cos-arg (base term1))))
               (let find-sin-partner ((remaining (cdr terms-to-scan)) (skipped-terms '()))
                 (cond
                   ((null? remaining) ; No sin^2(A) partner found
                    (search-pythagorean (cdr terms-to-scan) (cons term1 accumulated-non-identity-terms) ones-added))
                   ((and (power? (car remaining)) (equal? (exponent (car remaining)) 2) (sin? (base (car remaining)))
                         (equal? (sin-arg (base (car remaining))) argA))
                    ;; Partner sin^2(A) found!
                    (search-pythagorean (append (reverse skipped-terms) (cdr remaining)) accumulated-non-identity-terms (+ ones-added 1)))
                   (else ; Not a partner, keep searching
                    (find-sin-partner (cdr remaining) (cons (car remaining) skipped-terms)))))))

            ;; Default: term1 is not part of a recognized Pythagorean pair start
            (else
             (search-pythagorean (cdr terms-to-scan) (cons term1 accumulated-non-identity-terms) ones-added)))))))

(define (simplify-sum expr)
  (let* ((initial-operands (operands expr)))
    (let main-sum-loop ((current-operands-list initial-operands) (iteration-count 0))
      (if (> iteration-count 10)
          (error "simplify-sum: Exceeded iteration limit for expression" expr)
          (let* ((simplified-terms (map simplify current-operands-list))
                 (flat-terms (flatten-sum-terms-iterative simplified-terms))
                 (negation-distribution-result
                  (let process-negations ((terms-to-scan flat-terms) (accumulated '()) (changed? #f))
                    ;; ... your existing negation distribution logic ...
                    ;; This helper would return (values new-terms changed?)
                    (if (null? terms-to-scan) (values (reverse accumulated) changed?)
                        (let ((current-term (car terms-to-scan)))
                          (if (and (negation? current-term) (sum? (negated-expr current-term)))
                              (process-negations (cdr terms-to-scan)
                                                 (append (reverse (map make-negation (operands (negated-expr current-term)))) accumulated)
                                                 #t)
                              (process-negations (cdr terms-to-scan) (cons current-term accumulated) changed?))))))
                 (terms-after-negation (car negation-distribution-result))
                 (negation-was-distributed? (cadr negation-distribution-result)))

            (if negation-was-distributed?
                (main-sum-loop terms-after-negation (+ iteration-count 1))
                
                ;; If no negations were distributed in this pass, proceed with term collection.
                ;; 'flat-terms' is the stable list of terms to work with.
                (let ((final-flat-terms flat-terms)) 
                  (if (null? final-flat-terms)
                      0 ; Sum of no terms is 0
                (let* (;; Step X: Apply identity rules to the list of terms
                       (identity-rule-result (apply-sum-identity-rules terms-after-negation))
                       (terms-after-identities (car identity-rule-result))
                       (constants-from-identities (cadr identity-rule-result))

                       ;; Step 3: Separate numeric constants and sum them.
                       (numeric-constant-terms (filter constant? terms-after-identities))
                       (other-terms (filter (lambda (t) (not (constant? t))) terms-after-identities))
                       ;; Add constants generated by identities (e.g., the '1's)
                       (numeric-sum (+ constants-from-identities
                                       (if (null? numeric-constant-terms) 0 (apply + numeric-constant-terms))))
                       
                             ;; Step 4: Group non-constant terms by their base and sum coefficients.
                             (term-groups
                              (let collect-groups ((terms-to-process other-terms) (groups '()))
                                (if (null? terms-to-process)
                                    groups
                                    (call-with-values (lambda () (get-coeff-base-term (car terms-to-process)))
                                      (lambda (coeff base)
                                        (let ((existing-entry (assoc base groups)))
                                          (if existing-entry
                                              (collect-groups (cdr terms-to-process)
                                                    (cons (cons base (+ (cdr existing-entry) coeff)) ; Coeffs must be numeric here
                                                          (filter (lambda (p) (not (eq? p existing-entry))) groups)))
                                              (collect-groups (cdr terms-to-process)
                                                    (cons (cons base coeff) groups)))))))))
                            ;; Step 5: Reconstruct terms from groups.
                            (new-non-constant-terms
                             (filter-map
                              (lambda (group)
                                (let ((base (car group)) (summed-coeff (cdr group)))
                                  (cond
                                    ((= summed-coeff 0) #f) ; Coefficient is 0, term vanishes
                                    ((equal? base 1) #f) ; Base was the dummy '1' for constants, already handled
                                    ((= summed-coeff 1) base)
                                    ((= summed-coeff -1) (make-negation base))
                                    ;; Re-simplify the new term, e.g. (* 2 x) or (* -2 x)
                                    (else (simplify (make-product (list summed-coeff base)))))))
                              term-groups))
                            ;; Step 6: Combine numeric sum with new terms and sort.
                            (final-terms-to-sum
                             (let ((terms-for-sorting
                                    (if (and (number? numeric-sum) (= numeric-sum 0) (not (null? new-non-constant-terms)))
                                        new-non-constant-terms ; Don't include 0 if other terms exist
                                        (cons numeric-sum new-non-constant-terms))))
                               ;; If numeric_sum is 0 and it's the only thing, it should remain.
                               ;; The above logic handles this: if new-non-constant-terms is null,
                               ;; (cons 0 '()) is '(0) which is correct.
                               (list-sort term<? terms-for-sorting))))
                        ;; Step 7: Construct the final sum expression.
                        (cond
                          ((null? final-terms-to-sum) 0)
                          ((null? (cdr final-terms-to-sum)) (car final-terms-to-sum))
                          (else (make-sum final-terms-to-sum))))))))))))

;; Helper to symbolically add two exponents.
;; Ensures that the result is simplified.
(define (add-exponents exp1 exp2)
  (cond
    ((and (number? exp1) (number? exp2)) (+ exp1 exp2))
    ((and (number? exp1) (zero? exp1)) exp2)
    ((and (number? exp2) (zero? exp2)) exp1)
    ;; If one is already a sum, append the other term and simplify
    ;; This helps build sums like (+ n m p) rather than (+ (+ n m) p) initially
    ((sum? exp1) (simplify (make-sum (append (operands exp1) (list exp2)))))
    ((sum? exp2) (simplify (make-sum (append (operands exp2) (list exp1)))))
    ;; Default: create a new sum and simplify
    (else (simplify (make-sum (list exp1 exp2))))))

;; Helper for simplify-product: groups sorted identical factors and combines powers,
;; handling symbolic exponents correctly.
;; Assumes sorted-factors is a list of factors already sorted by term<?.
;; E.g., (a a (^ a n) b (^ b m) c) -> ((^ a (+ 2 n)) (^ b (+ m 1)) c)
(define (group-factors-into-powers sorted-factors)
  (if (null? sorted-factors)
      '()
      (let* ((current-base-candidate (car sorted-factors))
             (current-base (if (power? current-base-candidate)
                               (base current-base-candidate)
                               current-base-candidate)))
        ;; Inner recursive helper to process all factors for the current_base
        (let collect-for-this-base ((factors-for-this-base-sequence sorted-factors)
                                     (accumulated-exponent-for-this-base 0))
          (if (or (null? factors-for-this-base-sequence)
                  (let ((next-factor-base-candidate (car factors-for-this-base-sequence)))
                    (not (equal? (if (power? next-factor-base-candidate)
                                     (base next-factor-base-candidate)
                                     next-factor-base-candidate)
                                 current-base))))
              ;; This base's sequence is done.
              (let* ((final-exponent (simplify accumulated-exponent-for-this-base))
                     (grouped-term
                      (simplify
                      (cond
                        ((and (number? final-exponent) (zero? final-exponent)) 1)
                        ((and (number? final-exponent) (= final-exponent 1)) current-base)
                        (else (list '^ current-base final-exponent))))))
                ;; Filter out the '1' terms unless it's the only term and the list was originally non-empty
                (let ((next-grouping (group-factors-into-powers factors-for-this-base-sequence)))
                  (if (and (equal? grouped-term 1) (not (null? next-grouping)))
                      next-grouping
                      (cons grouped-term next-grouping))))
              ;; Current factor matches current_base. Accumulate its exponent.
              (let* ((factor-to-process (car factors-for-this-base-sequence))
                     (exponent-of-this-factor (if (power? factor-to-process)
                                                  (exponent factor-to-process)
                                                  1)))
                (collect-for-this-base (cdr factors-for-this-base-sequence)
                                       (add-exponents accumulated-exponent-for-this-base exponent-of-this-factor))))))))

(define (flatten-product-factors-iterative factors-list)
  (let collect-flat ((input-factors factors-list) (output-factors '()))
    (if (null? input-factors)
        (reverse output-factors) ; Reverse to maintain relative order of original non-product items
        (let ((factor (car input-factors)))
          (if (product? factor)
              ;; If it's a product, prepend its operands to the list of factors still to process.
              ;; The operands of 'factor' should already be simplified and flattened if they were products.
              (collect-flat (append (operands factor) (cdr input-factors)) output-factors)
              ;; Otherwise, add it to the accumulated output factors.
              (collect-flat (cdr input-factors) (cons factor output-factors)))))))

(define (apply-product-identity-rules-to-factors current-const-val non-constant-factors-list)
  (let* ((factors-with-const (if (equal? current-const-val 1)
                                 non-constant-factors-list
                                 (cons current-const-val non-constant-factors-list)))
         ;; --- Rule: 2 * sin(A) * cos(A) -> sin(2A) ---
         ;; This needs to find 2, sin(A), and cos(A) in factors-with-const
         (found-2 (find-if (lambda (f) (equal? f 2)) factors-with-const))
         (remaining-after-2 (if found-2 (remove 2 factors-with-const) factors-with-const)))

    (if found-2
        (let* ((found-sinA #f) (argA #f) (remaining-after-sinA remaining-after-2))
          ;; Find sin(A)
          (set! found-sinA (find-if (lambda (f) (sin? f)) remaining-after-2))
          (if found-sinA
              (begin
                (set! argA (sin-arg found-sinA))
                (set! remaining-after-sinA (remove found-sinA remaining-after-2)))
              #f) ; sin(A) not found

          (if (and found-sinA argA)
              (let ((found-cosA-partner (find-if (lambda (f) (and (cos? f) (equal? (cos-arg f) argA))) remaining-after-sinA)))
                (if found-cosA-partner
                    ;; Pattern found!
                    (let* ((remaining-factors (remove found-cosA-partner remaining-after-sinA))
                           (new-term (make-sin (make-product (list 2 argA))))
                           ;; Separate the new constant and non-constant parts from remaining-factors
                           (new-const-val-from-remaining (apply * (filter constant? remaining-factors)))
                           (new-non-const-from-remaining (filter (lambda (f) (not (constant? f))) remaining-factors)))
                      (values new-const-val-from-remaining (cons new-term new-non-const-from-remaining)))
                    ;; cos(A) partner not found
                    (values current-const-val non-constant-factors-list))) ; Return original
              ;; sin(A) not found after finding 2
              (values current-const-val non-constant-factors-list))) ; Return original
        ;; 2 not found
        (values current-const-val non-constant-factors-list)))) ; Return original

;; Simplification for products (with flattening, ordering, and power-grouping)
;; Simplification for products (with flattening, ordering, and power-grouping)
(define (simplify-product expr)
  (let* ((raw-operands (operands expr))
         (simplified-operands (map simplify raw-operands))
         (flat-initial-factors (flatten-product-factors-iterative simplified-operands)))
    (if (member 0 flat-initial-factors)
        0
        (let* ((numeric-constants (filter constant? flat-initial-factors))
               (other-initial-factors (filter (lambda (f) (not (constant? f))) flat-initial-factors))
               (base-const-val (if (null? numeric-constants) 1 (apply * numeric-constants))))
          (if (and (number? base-const-val) (= base-const-val 0))
              0
              (call-with-values
               (lambda ()
                 (let loop ((current-const base-const-val)
                            (remaining-others other-initial-factors)
                            (acc-non-const '()))
                   (if (null? remaining-others)
                       (values current-const (reverse acc-non-const))
                       (let ((factor (car remaining-others)))
                         (if (negation? factor)
                             (loop (* current-const -1)
                                   (cdr remaining-others)
                                   (cons (negated-expr factor) acc-non-const))
                             (loop current-const
                                   (cdr remaining-others)
                                   (cons factor acc-non-const)))))))
               (lambda (intermediate-const-val processed-non-constants-before-rules)
                 ;; --- APPLY PRODUCT IDENTITY RULES HERE ---
                 (call-with-values
                  (lambda () (apply-product-identity-rules-to-factors intermediate-const-val processed-non-constants-before-rules))
                  (lambda (const-after-rules factors-after-rules)
                    ;; --- END APPLY PRODUCT IDENTITY RULES ---
                    (let* ((sorted-factors-before-grouping (list-sort term<? factors-after-rules))
                           (grouped-terms (group-factors-into-powers sorted-factors-before-grouping))
                           ;; apply-product-contraction-rules is removed from here, handled above
                           (final-non-constant-factors-unsorted (filter (lambda (f) (and (not (constant? f)) (not (equal? f 1)))) grouped-terms))
                           (final-non-constant-factors (list-sort term<? final-non-constant-factors-unsorted))
                           (newly-formed-constants (filter constant? grouped-terms))
                           (final-const-val (apply * const-after-rules newly-formed-constants)))
                      (cond
                        ((and (number? final-const-val) (= final-const-val 0)) 0)
                        ((null? final-non-constant-factors) final-const-val)
                        ((and (number? final-const-val) (= final-const-val 1))
                         (if (null? (cdr final-non-constant-factors))
                             (car final-non-constant-factors)
                             (make-product final-non-constant-factors)))
                        ((and (number? final-const-val) (= final-const-val -1))
                         (if (null? (cdr final-non-constant-factors))
                             (make-negation (car final-non-constant-factors))
                             (make-negation (make-product final-non-constant-factors))))
                        (else
                         (make-product (cons final-const-val final-non-constant-factors))))))))))))))

(define (simplify-abs expr)
  (let ((arg (simplify (abs-arg expr)))) ; Simplify argument first
    (cond
      ;; NEW Rule: abs(abs(Y)) -> abs(Y)
      ((abs? arg) arg)

      ;; Rule 1: abs(constant) -> |constant|
      ((constant? arg) (abs arg)) ; Scheme's built-in abs for numbers

      ;; Rule 1.5: abs(i) -> 1
      ((eq? arg 'i) 1)

      ;; Rule 2: abs((- inner_expr)) -> (abs inner_expr)
      ((negation? arg) (simplify (make-abs (negated-expr arg))))

      ;; Rule 3: abs((^ base P_EVEN)) -> (^ base P_EVEN)
      ;; This applies if base^P_EVEN is non-negative.
      ;; Example: abs((^ y 2)) -> (^ y 2)
      ;; Assumes base is not 'i' to avoid abs(i^2)=1 vs i^2=-1.
      ;; If base were a constant, arg would likely have simplified to a constant,
      ;; and abs(constant) would have caught it.
      ((and (power? arg)
            (let ((p-base (base arg)) (p-exp (exponent arg)))
              (and (integer? p-exp)
                   (even? p-exp)
                   (> p-exp 0)
                   (not (eq? p-base 'i))
                   ;; This rule is for when the structure itself implies non-negativity
                   ;; for real variables or non-problematic constants.
                   )))
       arg)
      
      ;; Default: no further simplification for abs, return (abs simplified_arg)
      (else (make-abs arg)))))

;; Revised simplify-power, renamed to simplify-power-core:
;; This will contain rules that are complex, procedural, or depend on original (unsimplified) sub-expression structure.
(define (simplify-power-core expr)
  (let ((original-base (base expr))
        (original-exponent (exponent expr)))
    (cond
      ;; Rule 4.5 (Kept in core due to complexity and use of original-base structure)
      ;; (^ (base_inner^N_inner) (1/D_outer)) where N_inner = D_outer
      ((and (power? original-base)
            (let ((simplified-outer-exp (simplify original-exponent)))
              (and (rational? simplified-outer-exp)
                   (= (numerator simplified-outer-exp) 1)
                   (let ((N_inner (exponent original-base))
                         (D_outer (denominator simplified-outer-exp)))
                     (and (integer? N_inner) (> N_inner 0)
                          (equal? N_inner D_outer))))))
       (let* ((base_inner (base original-base))
              (N_inner (exponent original-base)))
         (if (odd? N_inner)
             (simplify base_inner)
             (simplify (make-abs (simplify base_inner))))))

      ;; If Rule 4.5 didn't match, proceed with simplified base and exponent for other rules
      (else
       (let ((b (simplify original-base))
             (exp-val (simplify original-exponent)))
         (cond
           ;; 3. Rule: (^ -1 rational-exponent) - Kept in core
           ((and (equal? b -1) (rational? exp-val) (not (integer? exp-val)))
            (let ((N (numerator exp-val))
                  (D (denominator exp-val)))
              (if (odd? D)
                  (if (odd? N) -1 1)
                  (let ((k (/ D 2)))
                    (simplify (make-power 'i (/ N k)))))))

           ;; ADDED: Structural Rule: Power of a power (uses simplified b)
           ((power? b) ; 'b' here is (simplify original-base)
            (simplify (make-power (base b) (simplify (make-product (list (exponent b) exp-val))))))

           ;; 6. Structural Rule: Power of a product - Kept in core (hard for simple pattern matcher)
           ((product? b)
            (simplify (make-product (map (lambda (factor) (make-power factor exp-val)) (operands b)))))

           ;; 8. Negative Constant Base with Rational Exponent (neg-const != -1) - Kept in core
           ((and (constant? b) (< b 0) (not (equal? b -1)) (rational? exp-val) (not (integer? exp-val)))
            (let ((N (numerator exp-val))
                  (D (denominator exp-val))
                  (abs-b (abs b)))
              (if (and (even? D) (odd? N))
                  (simplify (make-product (list (make-power -1 exp-val)
                                                (make-power abs-b exp-val))))
                  (if (odd? D)
                      (let ((term-abs (simplify (make-power abs-b exp-val))))
                        (if (odd? N)
                            (simplify (make-negation term-abs))
                            term-abs))
                      (make-power b exp-val)))))

           ;; 9. Numeric Specific Rule: Simplifying roots of positive integers - Kept in core
           ((and (integer? b) (> b 0) (rational? exp-val) (not (integer? exp-val)))
            (let ((num-exp (numerator exp-val))
                  (den-exp (denominator exp-val)))
              (if (= b 1)
                  1
                  (let* ((prime-factor-list (prime-factors b)))
                    (if (null? prime-factor-list)
                        (make-power b exp-val)
                        (let* ((unique-primes (remove-duplicates prime-factor-list))
                               (grouped-prime-powers
                                (map (lambda (p) (cons p (count (lambda (x) (= x p)) prime-factor-list)))
                                     unique-primes))
                               (new-factors
                                (map (lambda (prime-power-pair)
                                       (let* ((prime (car prime-power-pair))
                                              (original-power (cdr prime-power-pair))
                                              (new-exponent-numerator (* original-power num-exp))
                                              (common-divisor (gcd new-exponent-numerator den-exp))
                                              (reduced-exp-num (/ new-exponent-numerator common-divisor))
                                              (reduced-exp-den (/ den-exp common-divisor)))
                                         (if (= reduced-exp-den 1)
                                             (make-power prime reduced-exp-num)
                                             (let* ((integer-part-of-exponent (quotient reduced-exp-num reduced-exp-den))
                                                    (fractional-numerator (remainder reduced-exp-num reduced-exp-den)))
                                               (cond
                                                 ((= fractional-numerator 0)
                                                  (make-power prime integer-part-of-exponent))
                                                 ((= integer-part-of-exponent 0)
                                                  (make-power prime (/ fractional-numerator reduced-exp-den)))
                                                 (else
                                                  (make-product (list (make-power prime integer-part-of-exponent)
                                                                      (make-power prime (/ fractional-numerator reduced-exp-den))))))))))
                                     grouped-prime-powers))
                               (product-of-new-factors (make-product new-factors))
                               (current-power-form (list '^ b exp-val)))
                          (if (equal? product-of-new-factors current-power-form)
                              current-power-form
                              (simplify product-of-new-factors))))))))
           ;; 10. Default: No specific simplification rule applied by core logic
           (else (make-power b exp-val))))))))


;; Helper to create a negation, possibly simplifying double negations
;; This is used by simplify-product when prod-const is -1
(define (make-negation expr)
  (if (negation? expr)
      (car (operands expr)) ; Corrected: (- (- x)) -> x (operand itself, not list)
      (list '- expr)))

;; Simplification for negations '(- x)'
(define (simplify-negation expr)
  (let ((s-op (simplify (car (operands expr))))) ; Corrected: car instead of cadr
    (cond
      ;; op is a number => compute -op
      ((constant? s-op) (- s-op))
      ;; op is already a negation '(- y)' => --y = y
      ((negation? s-op) (car (operands s-op))) ; Corrected: car instead of cadr
      ;; op is a product '(* c y)' where c is constant => '(* (- c) y)'
      ((and (product? s-op) (pair? (operands s-op)) (constant? (car (operands s-op))))
       (simplify (cons '* (cons (- (car (operands s-op))) (cdr (operands s-op))))))
      ;; Default case
      (else (list '- s-op)))))

;; Simplification for differences '(- a b)'
(define (simplify-difference expr)
  (let ((s-a (simplify (minuend expr)))
        (s-b (simplify (subtrahend expr))))
    (cond
      ;; Both are constants
      ((and (constant? s-a) (constant? s-b)) (- s-a s-b))
      ;; b is 0 => a - 0 = a
      ((and (constant? s-b) (zero? s-b)) s-a)
      ;; a is 0 => 0 - b = -b
      ((and (constant? s-a) (zero? s-a)) (simplify (list '- s-b))) ; simplify-negation will handle
      ;; a = b => a - a = 0
      ((equal? s-a s-b) 0)
      ;; Canonical form: (- a b) -> (+ a (* -1 b))
      (else (simplify (list '+ s-a (list '* -1 s-b)))))))

;; Core simplification logic for quotients (cases not covered by rules)
(define (simplify-quotient-core expr)
  (let ((s-num (simplify (quotient-numerator expr)))
        (s-den (simplify (quotient-denominator expr))))
    (cond
      ;; Denominator is 0 - division by zero
      ((and (constant? s-den) (zero? s-den)) (list '/ s-num s-den))
      ;; Both are constants (and denominator is not 0)
      ((and (constant? s-num) (constant? s-den) (not (zero? s-den))) (/ s-num s-den))
      ;; Default case for core logic if rules didn't handle it
      (else (list '/ s-num s-den)))))

;; Core simplification logic for exp (cases not covered by rules)
(define (simplify-exp-core expr)
  (let ((arg (simplify (exp-arg expr))))
    (cond
      ;; Rule: (exp (* ... (ln X) ...)) -> (^ X other-factors)
      ;; This is the more general version if the specific rule (exp (* ?k (ln ?x))) didn't match.
      ((product? arg)
       (let* ((factors (operands arg))
              (ln-factor (find-if ln? factors)))
         (if ln-factor
             (let ((inner-ln-arg (ln-arg ln-factor))
                   (other-factors (remove ln-factor factors)))
               (if (null? other-factors)
                   ;; This case (exp (ln X)) should have been caught by a rule.
                   ;; If it reaches here, it implies the rule wasn't specific enough or
                   ;; the expression was structured differently.
                   ;; For safety, returning inner-ln-arg is correct.
                   inner-ln-arg
                   (simplify (make-power inner-ln-arg (simplify (make-product other-factors))))))
             (make-exp arg)))) ; Product does not contain ln
      ;; Default for core logic
      (else (make-exp arg)))))

;; Core simplification logic for ln (cases not covered by rules)
(define (simplify-ln-core expr)
  (let ((arg (simplify (ln-arg expr))))
    (cond
      ;; Rule: (ln (exp x)) -> x (This was in original simplify-ln, if not caught by rule `(ln (exp ?x)) => ?x` due to structure)
      ;; It's generally better for rules to catch these. If it's here, it's a fallback.
      ((exp? arg)
       (exp-arg arg))
      ;; Rule: (ln (* f1 f2 ...)) -> (+ (ln f1) (ln f2) ...)
      ;; This rule is complex for the current simple rule engine due to variadic nature.
      ((product? arg)
       (let ((factors (operands arg)))
         (if (member 0 factors) ; (ln 0) is undefined.
             (make-ln arg)
             (simplify (make-sum (map make-ln factors))))))
      ;; Default for core logic
      (else (make-ln arg)))))

(define (simplify-sin expr)
  (let ((arg (simplify (sin-arg expr))))
    (cond
      ;; sin(0) -> 0
      ((equal? arg 0) 0)
      ;; sin(pi) -> 0
      ((equal? arg 'pi) 0)
      ;; sin(2*pi), sin(3*pi), etc. -> 0 (integer multiples of pi)
      ((and (product? arg)
            (equal? (length (operands arg)) 2)
            (let ((f1 (car (operands arg))) (f2 (cadr (operands arg))))
              (or (and (integer? f1) (equal? f2 'pi))
                  (and (integer? f2) (equal? f1 'pi)))))
       0)
      ;; sin(pi/2) -> 1
      ((equal? arg '(/ pi 2)) 1)
      ;; sin(pi/3) -> sqrt(3)/2
      ((equal? arg '(/ pi 3)) (simplify '(/ (^ 3 1/2) 2)))
      ;; sin(pi/4) -> sqrt(2)/2
      ((equal? arg '(/ pi 4)) (simplify '(/ (^ 2 1/2) 2)))
      ;; sin(pi/6) -> 1/2
      ((equal? arg '(/ pi 6)) 1/2)
      ;; sin(-x) -> -sin(x)
      ((negation? arg) (simplify (make-negation (make-sin (negated-expr arg)))))
      ;; Default
      (else (make-sin arg)))))

(define (simplify-cos expr)
  (let ((arg (simplify (cos-arg expr))))
    (cond
      ;; cos(0) -> 1
      ((equal? arg 0) 1)
      ;; cos(pi) -> -1
      ((equal? arg 'pi) -1)
      ;; cos(2*pi) -> 1 (even multiples of pi)
      ((and (product? arg)
            (equal? (length (operands arg)) 2)
            (let ((f1 (car (operands arg))) (f2 (cadr (operands arg))))
              (and (or (and (integer? f1) (even? f1) (equal? f2 'pi))
                       (and (integer? f2) (even? f2) (equal? f1 'pi))))))
       1)
      ;; cos(3*pi) -> -1 (odd multiples of pi, already covered by pi if simplified)
      ;; cos(pi/2) -> 0
      ((equal? arg '(/ pi 2)) 0)
      ;; cos(pi/3) -> 1/2
      ((equal? arg '(/ pi 3)) 1/2)
      ;; cos(pi/4) -> sqrt(2)/2
      ((equal? arg '(/ pi 4)) (simplify '(/ (^ 2 1/2) 2)))
      ;; cos(pi/6) -> sqrt(3)/2
      ((equal? arg '(/ pi 6)) (simplify '(/ (^ 3 1/2) 2)))
      ;; cos(-x) -> cos(x)
      ((negation? arg) (simplify (make-cos (negated-expr arg))))
      ;; Default
      (else (make-cos arg)))))

(define (simplify-tan expr)
  (let ((arg (simplify (tan-arg expr))))
    (cond
      ;; tan(0) -> 0
      ((equal? arg 0) 0)
      ;; tan(pi) -> 0
      ((equal? arg 'pi) 0)
      ;; tan(pi/4) -> 1
      ((equal? arg '(/ pi 4)) 1)
      ;; tan(pi/6) -> 1/sqrt(3)
      ((equal? arg '(/ pi 6)) (simplify '(/ 1 (^ 3 1/2))))
      ;; tan(pi/3) -> sqrt(3)
      ((equal? arg '(/ pi 3)) (simplify '(^ 3 1/2)))
      ;; tan(pi/2) is undefined, so return original if no other rule applies
      ((equal? arg '(/ pi 2)) (make-tan arg))
      ;; tan(-x) -> -tan(x)
      ((negation? arg) (simplify (make-negation (make-tan (negated-expr arg)))))
      ;; Default
      (else (make-tan arg)))))

(define MAX-SIMPLIFY-ITERATIONS 20) ; Example value, adjust as needed
;; Main simplify function (Modified to use the rule engine)
(set! simplify
  (lambda (initial-expr)
    (let loop ((current-expr initial-expr) (iteration-count 0))
      (if (> iteration-count MAX-SIMPLIFY-ITERATIONS) ; MAX-SIMPLIFY-ITERATIONS e.g., 20-50
          (begin
            (display "Warning: Simplification iteration limit reached for: ")
            (display initial-expr) (newline)
            current-expr) ; Return current state to avoid infinite loops

          (let* (;; --- Pass 1: Apply Rules to current_expr ---
                 (rule-application-result (apply-rules current-expr *simplify-rules*))
                 (expr-after-rules (car rule-application-result))
                 (rule-applied? (cdr rule-application-result)))

            (if rule-applied?
                ;; If a rule applied, the expression changed. Loop again with the new expression.
                (loop expr-after-rules (+ iteration-count 1))
                ;; --- No rule applied in this pass. Try structural simplification. ---
                (let ((structurally-simplified-expr
                       (cond
                         ((atomic-expr? expr-after-rules) expr-after-rules) ; Was current-expr, now expr-after-rules
                         ((sum? expr-after-rules) (simplify-sum expr-after-rules))
                         ((product? expr-after-rules) (simplify-product expr-after-rules))
                         ((power? expr-after-rules) (simplify-power-core expr-after-rules))
                         ((negation? expr-after-rules) (simplify-negation expr-after-rules))
                         ((difference? expr-after-rules) (simplify-difference expr-after-rules))
                         ((quotient? expr-after-rules) (simplify-quotient-core expr-after-rules))
                         ((exp? expr-after-rules) (simplify-exp-core expr-after-rules))
                         ((ln? expr-after-rules) (simplify-ln-core expr-after-rules))
                         ((sqrt? expr-after-rules) (simplify (make-power (sqrt-arg expr-after-rules) 1/2))) ; This simplify is for the new structure
                         ((abs? expr-after-rules) (simplify-abs expr-after-rules))
                         ((sin? expr-after-rules) (simplify-sin expr-after-rules))
                         ((cos? expr-after-rules) (simplify-cos expr-after-rules))
                         ((tan? expr-after-rules) (simplify-tan expr-after-rules))
                         ((expt? expr-after-rules) ; Added case for expt
                          (let ((b (simplify (expt-base expr-after-rules)))
                                (e (simplify (expt-exponent expr-after-rules))))
                            (if (and (number? b) (number? e))
                                (expt b e) ; Use Scheme's built-in expt for numbers
                                (list 'expt b e)))) ; Return symbolic if args not numeric
                         ((and (pair? expr-after-rules) (symbol? (car expr-after-rules)))
                          (cons (car expr-after-rules) (map simplify (cdr expr-after-rules))))
                         (else expr-after-rules)))) ; Corrected: 4 closing parentheses
                  ;; Body of the let:
                  (if (equal? structurally-simplified-expr expr-after-rules)
                      ;; Fixed point: No rule applied, and structural simplification didn't change it.
                      structurally-simplified-expr
                      ;; Structural simplification changed it. Loop again.
                      (loop structurally-simplified-expr (+ iteration-count 1))) ; Closes inner if
                  ) ; Closes the let
                ) ; Closes the (if rule-applied?)
                )))))

;;; --- Phase 2: Expansion (expand) ---

;; Forward declaration for expand
(define expand #f)

;; Helper: Applies distributive property to a product if one of its factors is a sum.
;; product-expr is an expression like '(* f1 f2 sum f3 ...)' where operands are already expanded.
;; Returns a new sum if distribution occurs, otherwise returns the original product-expr.
(define (expand-product-distributive product-expr)
  (let ((factors (operands product-expr)))
    (let find-sum-and-distribute ((current-factors factors) (factors-processed '()))
      (cond
        ((null? current-factors) product-expr) ; No sum found, or no suitable sum
        ((sum? (car current-factors))
         (let* ((sum-to-distribute (car current-factors))
                (terms-of-sum (operands sum-to-distribute))
                (other-factors (append (reverse factors-processed) (cdr current-factors))))
           (if (null? other-factors) ; Product was just `(* (+ a b))`, which simplify should handle.
               product-expr          ; No distribution to perform here.
               ;; Create the new sum: (+ (* term1 other_factors) (* term2 other_factors) ...)
               (make-sum (map (lambda (term)
                                ;; Each new product term is constructed.
                                ;; The main 'expand' will recursively expand these terms.
                                (make-product (cons term other-factors)))
                              terms-of-sum)))))
        (else (find-sum-and-distribute (cdr current-factors)
                                       (cons (car current-factors) factors-processed)))))))


;; Helper to flatten a list of lists by one level
(define (flatten-once lst)
  (apply append lst))

;; Helper to expand (* expr1 expr2) where expr1 and expr2 might be sums.
;; Returns a new sum expression (list of product terms).
;; This result is NOT yet fully simplified by simplify-sum (e.g., like terms not collected).
;; The main 'expand' function's simplify step will handle that.
(define (expand-product-of-two-expressions expr1 expr2)
  (let ((sum1 (if (sum? expr1) expr1 (make-sum (list expr1)))) ; Treat non-sum as sum of one term
        (sum2 (if (sum? expr2) expr2 (make-sum (list expr2)))))
    (let ((terms1 (operands sum1))
          (terms2 (operands sum2)))
      (make-sum
       (flatten-once ; To avoid nested lists from (map (lambda (t1) (map ...)))
        (map (lambda (t1)
               (map (lambda (t2)
                      ;; Create product term. The main 'expand' will simplify these products.
                      (make-product (list t1 t2)))
                    terms2))
             terms1))))))

;; Revised Arbitrary polynomial expansion for sums of powers
;; This function is called from expand-power-rules.
;; It returns an expanded form; the main 'expand' function will then simplify it.
(define (expand-polynomial-sum power-expr)
  ;; power-expr is '(^ base exponent)'
  ;; Conditions like (sum? base), (integer? exponent), (> exponent 1)
  ;; are assumed to be checked by the caller (expand-power-rules).
  (let ((base (base power-expr))
        (exponent (exponent power-expr)))
    ;; Inner recursive worker.
    ;; Returns an expanded sum (or 1 if exponent is 0, or base if exponent is 1).
    (define (recursive-worker current-base current-exponent)
      (cond ((= current-exponent 0) 1)
            ((= current-exponent 1) current-base)
            (else
             ;; Perform (current-base * result-of-recursive-call)
             (expand-product-of-two-expressions current-base
                                                (recursive-worker current-base (- current-exponent 1))))))
    (recursive-worker base exponent)))

(define (factorial n) (if (<= n 1) 1 (apply * (iota n n -1)))) 

(define (combinations n k)
  (if (or (< k 0) (> k n)) 0
      (/ (factorial n) (* (factorial k) (factorial (- n k))))))

;; Expands (^ (term1 + term2) exponent) using binomial theorem.
;; Returns a sum expression.
(define (expand-binomial-sum term1 term2 exponent)
  (make-sum
   (filter-map ; filter-map to remove potential nils if a term becomes 0 or coeff is 0
    (lambda (k)
      (let ((coeff (combinations exponent k)))
        (if (zero? coeff)
            #f ; This term is zero
            (let* ((n-k (- exponent k))
                   (pow1 (cond ((= n-k 0) 1) ((= n-k 1) term1) (else (list '^ term1 n-k))))
                   (pow2 (cond ((= k 0) 1) ((= k 1) term2) (else (list '^ term2 k))))
                  ;; term-parts will be a list like (coeff pow1 pow2), after filtering 1s
                  (term-parts (filter (lambda (p) (not (equal? p 1))) (list coeff pow1 pow2))))
              (cond ((null? term-parts) 1) ; e.g. for 1*1*1 if all were 1
                    ((null? (cdr term-parts)) (car term-parts)) ; Only one part left
                    ;; Corrected: Pass term-parts directly to make-product
                    (else (make-product term-parts))))))) ; Make product of remaining parts
    (iota (+ exponent 1) 0 1)))) ; k from 0 to exponent

;; Helper: Applies expansion rules for powers.
;; power-expr is like '(^ base exponent)' where base and exponent are already expanded.
;; Returns a new expanded expression or the original power-expr if no rule applies.
(define (expand-power-rules power-expr)
  (let ((base (base power-expr))
        (exponent (exponent power-expr)))
    (cond
      ;; Rule 0a: Exponent is 0 (integer)
      ((and (integer? exponent) (= exponent 0)) 1)
      ;; Rule 0b: Exponent is 1 (integer)
      ((and (integer? exponent) (= exponent 1)) base)

      ;; Rule A: Binomial expansion for (^ (+ t1 t2) n), where n is an integer >= 2
      ((and (sum? base)
            (integer? exponent)
            (>= exponent 2)
            (= (length (operands base)) 2))
       (expand-binomial-sum (car (operands base)) (cadr (operands base)) exponent))

      ;; Rule B: General polynomial sum expansion for (^ (sum) n), where n is an integer >= 2
      ((and (sum? base)
            (integer? exponent)
            (>= exponent 2))
       (expand-polynomial-sum power-expr))

      ;; Rule C: Power of a Product (^ (* f1 f2) e) -> (* (^ f1 e) (^ f2 e))
      ;; This rule applies whether 'e' is an integer or symbolic.
      ((product? base)
       (make-product (map (lambda (factor) (list '^ factor exponent))
                          (operands base))))

      ;; Rule D: Power of a Quotient (^ (/ n d) e) -> (/ (^ n e) (^ d e))
      ;; This rule applies whether 'e' is an integer or symbolic.
      ((quotient? base)
       (list '/
             (list '^ (quotient-numerator base) exponent)
             (list '^ (quotient-denominator base) exponent)))

      ;; Default: No expansion rule applied for the power expression itself
      (else power-expr))))

(define (expand-sin-expression sin-expr) ; sin-expr is like (sin expanded-arg)
  (let ((arg (sin-arg sin-expr))) ; Argument is already expanded
    (cond
      ;; sin(A+B) -> sin(A)cos(B)+cos(A)sin(B)
      ((and (sum? arg) (= (length (operands arg)) 2))
       (let ((termA (car (operands arg))) (termB (cadr (operands arg))))
         ;; Recursively call expand on the new structure, then simplify
         (expand (make-sum (list (make-product (list (make-sin termA) (make-cos termB)))
                                 (make-product (list (make-cos termA) (make-sin termB))))))))

      ;; sin(2A) which is sin (* 2 A)
      ((and (product? arg) (= (length (operands arg)) 2) (equal? (car (operands arg)) 2))
       (let ((termA (cadr (operands arg))))
         (expand (make-product (list 2 (make-sin termA) (make-cos termA))))))
      
      ;; Default: no specific expansion rule for sin's argument structure
      (else sin-expr))))

(define (expand-cos-expression cos-expr) ; cos-expr is like (cos expanded-arg)
  (let ((arg (cos-arg cos-expr))) ; Argument is already expanded
    (cond
      ;; cos(A+B) -> cos(A)cos(B)-sin(A)sin(B)
      ((and (sum? arg) (= (length (operands arg)) 2))
       (let ((termA (car (operands arg))) (termB (cadr (operands arg))))
         ;; Recursively call expand on the new structure, then simplify
         (expand (make-sum (list (make-product (list (make-cos termA) (make-cos termB)))
                                 (make-product (list -1
                                                     (make-sin termA)
                                                     (make-sin termB))))))))

      ;; cos(2A) which is cos (* 2 A)
      ((and (product? arg) (= (length (operands arg)) 2) (equal? (car (operands arg)) 2))
       (let ((termA (cadr (operands arg))))
         ;; cos(2A) = cos^2(A) - sin^2(A)
         (expand (make-sum (list (make-product (list (make-cos termA) (make-cos termA)))
                                 (make-product (list -1
                                                     (make-sin termA)
                                                     (make-sin termA))))))))

      ;; Default: no specific expansion rule for cos's argument structure
      (else cos-expr))))

(define (expand-tan-expression tan-expr) ; tan-expr is like (tan expanded-arg)
  (let ((arg (tan-arg tan-expr))) ; Argument is already expanded
    (cond
      ;; tan(A+B) -> (/ (+ (tan A) (tan B)) (- 1 (* (tan A) (tan B))))
      ((and (sum? arg) (= (length (operands arg)) 2))
       (let ((termA (car (operands arg))) (termB (cadr (operands arg))))
         ;; Recursively call expand on the new structure
         (expand (list '/
                       (make-sum (list (make-tan termA) (make-tan termB)))
                       (make-sum (list 1 (make-product (list -1 (make-tan termA) (make-tan termB)))))))))

      ;; tan(2A) which is tan (* 2 A) -> (/ (* 2 (tan A)) (- 1 (^ (tan A) 2)))
      ((and (product? arg) (= (length (operands arg)) 2) (equal? (car (operands arg)) 2))
       (let ((termA (cadr (operands arg))))
         ;; Recursively call expand on the new structure
         (expand (list '/
                       (make-product (list 2 (make-tan termA)))
                       (make-sum (list 1 (make-product (list -1 (make-power (make-tan termA) 2)))))))))
      
      ;; Default: no specific expansion rule for tan's argument structure
      (else tan-expr))))

;; Main expand function
;; Recursively expands expressions and simplifies the result.
(set! expand
  (lambda (expr)
    (if (atomic-expr? expr)
        expr ; Atomic expressions are considered fully expanded.
        (let* (;; Step 1: Recursively expand all operands first.
               (expanded-operands (map expand (operands expr)))
               (expr-with-expanded-operands (cons (operator expr) expanded-operands))

               ;; Step 2: Apply expansion rules to the current expression form.
               (rule-applied-expr
                (cond
                  ((sin? expr-with-expanded-operands) (expand-sin-expression expr-with-expanded-operands))
                  ((cos? expr-with-expanded-operands) (expand-cos-expression expr-with-expanded-operands))
                  ((tan? expr-with-expanded-operands) (expand-tan-expression expr-with-expanded-operands)) ; Added
                  ((product? expr-with-expanded-operands) (expand-product-distributive expr-with-expanded-operands))
                  ((power? expr-with-expanded-operands) (expand-power-rules expr-with-expanded-operands))
                  ;; Add other top-level expansion rules here if any (e.g., for sums, though not typical for "expansion")
                  (else expr-with-expanded-operands))))

          ;; Step 3: If a rule transformed the expression, the new expression might need further expansion.
          ;; Recursively call expand on the transformed expression.
          ;; Finally, simplify the result.
          (let ((result-before-final-simplify
                 (if (equal? rule-applied-expr expr-with-expanded-operands)
                     rule-applied-expr ; No rule applied at this level, or rule resulted in the same structure
                     (expand rule-applied-expr)))) ; Rule applied, so recursively expand the new structure
            (simplify result-before-final-simplify))))))

