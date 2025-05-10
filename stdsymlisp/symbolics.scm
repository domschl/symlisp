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
(define *known-operators* '(+ - * / ^ abs sin cos tan log exp sqrt)) ; Add more as needed

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

;; Helper to construct an abs expression
(define (make-abs arg)
  (list 'abs arg))

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


(define (simplify-sum expr)
  (let* ((initial-operands (operands expr)))
    ;; Iteratively process the sum until no more structural changes from negation distribution occur.
    (let main-sum-loop ((current-operands-list initial-operands) (iteration-count 0))
      (if (> iteration-count 10) ; Safeguard against potential infinite loops
          (error "simplify-sum: Exceeded iteration limit for expression" expr)
          (let* (;; Step 1: Simplify each operand individually.
                 (simplified-terms (map simplify current-operands-list))
                 ;; Step 2: Flatten the list of terms. E.g., (x (+ y z)) -> (x y z)
                 (flat-terms (flatten-sum-terms-iterative simplified-terms))
                 
                 ;; Step 2.5: Distribute negations over sums.
                 ;; E.g., (- (+ a b)) becomes (- a) (- b) in the list of terms.
                 ;; This returns (values new-term-list changed?)
                 (negation-distribution-result
                  (let process-negations ((terms-to-scan flat-terms) (accumulated-new-terms '()) (any-change-made? #f))
                    (if (null? terms-to-scan)
                        (values (reverse accumulated-new-terms) any-change-made?)
                        (let ((current-term (car terms-to-scan)))
                          (if (and (negation? current-term) (sum? (negated-expr current-term)))
                              (let* ((sum-inside-negation (negated-expr current-term))
                                     ;; Create (-t1), (-t2), ... for each term in the sum
                                     (distributed-negated-terms 
                                      (map make-negation (operands sum-inside-negation))))
                                ;; Prepend these new terms (reversed, to maintain order relative to each other)
                                ;; to the accumulated list and continue scanning the rest of the original terms.
                                (process-negations (cdr terms-to-scan)
                                                   (append (reverse distributed-negated-terms) accumulated-new-terms)
                                                   #t)) ; Mark that a change was made
                              ;; No distribution for this term, just add it to accumulated.
                              (process-negations (cdr terms-to-scan)
                                                 (cons current-term accumulated-new-terms)
                                                 any-change-made?))))))
                 (terms-after-negation-distribution (car negation-distribution-result))
                 (negation-was-distributed? (cadr negation-distribution-result)))

            (if negation-was-distributed?
                ;; If negations were distributed, the structure changed.
                ;; The new list of terms (terms-after-negation-distribution) needs to be re-simplified
                ;; and the whole sum simplification process restarted with these terms.
                (main-sum-loop terms-after-negation-distribution (+ iteration-count 1))
                
                ;; If no negations were distributed in this pass, proceed with term collection.
                ;; 'flat-terms' is the stable list of terms to work with.
                (let ((final-flat-terms flat-terms)) 
                  (if (null? final-flat-terms)
                      0 ; Sum of no terms is 0
                      (let* (;; Step 3: Separate numeric constants and sum them.
                             (numeric-constant-terms (filter constant? final-flat-terms))
                             (other-terms (filter (lambda (t) (not (constant? t))) final-flat-terms))
                             (numeric-sum (if (null? numeric-constant-terms) 0 (apply + numeric-constant-terms)))
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

;; Simplification for products (with flattening, ordering, and power-grouping)
(define (simplify-product expr)
  (let* ((raw-operands (operands expr))
         ;; Step 1: Simplify each operand individually.
         (simplified-operands (map simplify raw-operands))
         ;; Step 2: Flatten nested products.
         (flat-initial-factors (flatten-product-factors-iterative simplified-operands)))

    ;; Step 3: Check for 0 as a factor. If present, the whole product is 0.
    (if (member 0 flat-initial-factors)
        0
        (let* (;; Step 4: Separate initial numeric constants from other (non-constant) factors.
               (numeric-constants (filter constant? flat-initial-factors))
               (other-initial-factors (filter (lambda (f) (not (constant? f))) flat-initial-factors))
               (base-const-val (if (null? numeric-constants) 1 (apply * numeric-constants))))

          ;; If the initial constant part is 0 (e.g., from (* 0 x)), the whole product is 0.
          ;; This check is somewhat redundant due to the member check above, but harmless.
          (if (and (number? base-const-val) (= base-const-val 0))
              0
              ;; Step 5: Process non-constant factors to pull out negations and update constant.
              (call-with-values
               (lambda () ; Producer for intermediate-const-val and processed-non-constants-before-grouping
                 (let loop ((current-const base-const-val)
                            (remaining-others other-initial-factors)
                            (acc-non-const '()))
                   (if (null? remaining-others)
                       (values current-const (reverse acc-non-const))
                       (let ((factor (car remaining-others)))
                         (if (negation? factor) ; e.g., (- x)
                             (loop (* current-const -1)
                                   (cdr remaining-others)
                                   (cons (negated-expr factor) acc-non-const)) ; Add x
                             (loop current-const
                                   (cdr remaining-others)
                                   (cons factor acc-non-const)))))))
               (lambda (intermediate-const-val processed-non-constants-before-grouping)
                 ;; Step 6: Sort the remaining non-constant factors for canonical grouping.
                 (let* ((sorted-factors-before-grouping (list-sort term<? processed-non-constants-before-grouping))
                        ;; Step 7: Group identical factors into powers.
                        ;; group-factors-into-powers now simplifies the terms it creates.
                        ;; So, (^ i 2) becomes -1 here.
                        (grouped-terms (group-factors-into-powers sorted-factors-before-grouping))

                        ;; Step 8: Re-evaluate constants and sort final non-constant factors.
                        ;; Some grouped terms might have become constants (e.g., (^ i 2) -> -1).
                        ;; Also, filter out any '1's that resulted from grouping (e.g. x^0 -> 1)
                        (final-non-constant-factors-unsorted (filter (lambda (f) (and (not (constant? f)) (not (equal? f 1)))) grouped-terms))
                        (final-non-constant-factors (list-sort term<? final-non-constant-factors-unsorted)) ; Added sort here
                        (newly-formed-constants (filter constant? grouped-terms))
                        
                        (final-const-val (apply * intermediate-const-val newly-formed-constants)))

                   ;; Step 9: Construct the final simplified product.
                   (cond
                     ((and (number? final-const-val) (= final-const-val 0)) 0)
                     ((null? final-non-constant-factors)
                      ;; If no non-constant factors left, the result is just the constant value.
                      ;; (This handles cases like (* 1), (* -1), (* 5), (* i i) -> -1)
                      final-const-val)
                     ((and (number? final-const-val) (= final-const-val 1))
                      (if (null? (cdr final-non-constant-factors)) ; Only one non-constant factor
                          (car final-non-constant-factors)
                          (make-product final-non-constant-factors)))
                     ((and (number? final-const-val) (= final-const-val -1))
                      (if (null? (cdr final-non-constant-factors))
                          (make-negation (car final-non-constant-factors))
                          (make-negation (make-product final-non-constant-factors))))
                     (else ; Constant is other than 0, 1, -1, or non-constant factors exist.
                      (make-product (cons final-const-val final-non-constant-factors))))))))))))

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

;; Revised simplify-power:
(define (simplify-power expr)
  (let ((original-base (base expr))
        (original-exponent (exponent expr)))
    (cond
      ;; Rule 4.5 (Moved to top & modified to use original-base structure)
      ;; (^ (base_inner^N_inner) (1/D_outer)) where N_inner = D_outer
      ((and (power? original-base) ; Check structure of original-base
            (let ((simplified-outer-exp (simplify original-exponent))) ; Simplify outer exponent for checks
              (and (rational? simplified-outer-exp)
                   (= (numerator simplified-outer-exp) 1) ; Outer exponent is 1/D_outer
                   (let ((N_inner (exponent original-base)) ; From original-base's structure
                         (D_outer (denominator simplified-outer-exp)))
                     ;; N_inner must be a positive integer, and N_inner must equal D_outer
                     (and (integer? N_inner) (> N_inner 0)
                          (equal? N_inner D_outer))))))
       (let* ((base_inner (base original-base))  ; Innermost base from original structure
              (N_inner (exponent original-base))) ; N_inner from original structure
         (if (odd? N_inner)
             (simplify base_inner) ; Simplify the innermost base before returning
             (simplify (make-abs (simplify base_inner)))))) ; Simplify arg of abs, then simplify abs expr itself

      ;; If Rule 4.5 didn't match, proceed with simplified base and exponent for other rules
      (else
       (let ((b (simplify original-base)) ; Now simplify base for other rules
             (exp-val (simplify original-exponent))) ; and exponent for other rules
         (cond
           ;; 1. Constant folding for INTEGER exponents
           ((and (constant? b) (constant? exp-val) (integer? exp-val))
            (if (and (number? b) (= b 0) (<= exp-val 0))
                (if (= exp-val 0) 1 (error "simplify-power: 0 to a negative power is undefined"))
                (expt b exp-val)))

           ;; 2. Rule for i^n where n is an integer
           ((and (eq? b 'i) (integer? exp-val))
            (let ((rem (modulo exp-val 4)))
              (cond
                ((= rem 0) 1)
                ((= rem 1) 'i)
                ((= rem 2) -1)
                ((= rem 3) (make-negation 'i))
                (else (make-power b exp-val)))))

           ;; 3. Rule: (^ -1 rational-exponent)
           ((and (equal? b -1) (rational? exp-val) (not (integer? exp-val)))
            (let ((N (numerator exp-val))
                  (D (denominator exp-val)))
              (if (odd? D)
                  (if (odd? N) -1 1) 
                  (let ((k (/ D 2))) 
                    (simplify (make-power 'i (/ N k)))))))

           ;; 4. Standard identities
           ((and (constant? exp-val) (= exp-val 0)) 1)
           ((and (constant? exp-val) (= exp-val 1)) b)
           ((and (constant? b) (= b 0))
            (if (and (constant? exp-val) (> exp-val 0)) 0 (make-power b exp-val)))
           ((and (constant? b) (= b 1)) 1)

           ;; 5. Structural Rule: Power of a power (uses simplified b)
           ((power? b) ; 'b' here is (simplify original-base)
            (simplify (make-power (base b) (simplify (make-product (list (exponent b) exp-val))))))

           ;; 6. Structural Rule: Power of a product
           ((product? b)
            (simplify (make-product (map (lambda (factor) (make-power factor exp-val)) (operands b)))))
           
           ;; 7. Structural Rule: Power of a negation (integer exponent)
           ((and (negation? b) (integer? exp-val) (>= exp-val 0))
            (let ((base-of-negation (negated-expr b)))
              (if (even? exp-val)
                  (simplify (make-power base-of-negation exp-val))
                  (simplify (make-negation (make-power base-of-negation exp-val))))))

           ;; 8. Negative Constant Base with Rational Exponent (neg-const != -1)
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

           ;; 9. Numeric Specific Rule: Simplifying roots of positive integers
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
           
           ;; 10. Default: No specific simplification rule applied
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

;; Simplification for quotients '(/ a b)'
(define (simplify-quotient expr)
  (let ((s-num (simplify (quotient-numerator expr)))
        (s-den (simplify (quotient-denominator expr))))
    (cond
      ;; Denominator is 0 - division by zero (return unsimplified or error)
      ((and (constant? s-den) (zero? s-den)) (list '/ s-num s-den)) ; Or (error "Division by zero" expr)
      ;; Numerator is 0 (and denominator is not)
      ((and (constant? s-num) (zero? s-num)) 0)
      ;; Both are constants (and denominator is not 0)
      ((and (constant? s-num) (constant? s-den)) (/ s-num s-den))
      ;; Denominator is 1
      ((and (constant? s-den) (= s-den 1)) s-num)
      ;; Numerator and denominator are equal (and not zero)
      ((equal? s-num s-den) 1)
      ;; Default case
      (else (list '/ s-num s-den)))))


;; Main simplify function
(set! simplify
      (lambda (expr)
        (cond
          ;; 1. Atomic expressions (constants, variables) are already simple.
          ((atomic-expr? expr) expr)

          ;; 2. Specific known operators with dedicated simplification logic.
          ((sum? expr) (simplify-sum expr))
          ((product? expr) (simplify-product expr))
          ((power? expr) (simplify-power expr))
          ((negation? expr) (simplify-negation expr))
          ((difference? expr) (simplify-difference expr))
          ((quotient? expr) (simplify-quotient expr))
          
          ;; Rule for sqrt: (sqrt x) -> (^ x 1/2) for symbolic processing
          ((sqrt? expr) (simplify (make-power (sqrt-arg expr) 1/2)))

          ;; Rule for abs
          ((abs? expr) (simplify-abs expr))

          ;; ... add other specific functions like (simplify-sin expr) if they have rules ...

          ;; 3. General case for expressions that look like function calls:
          ;;    (function-symbol arg1 arg2 ...)
          ;;    Simplify arguments. This applies to:
          ;;    a) Known operators from *known-operators* that don't have a specific
          ;;       simplify-X rule defined above (e.g., 'sin' if simplify-sin isn't written yet).
          ;;    b) Unknown/user-defined functions (like 'foo').
          ((and (pair? expr) (symbol? (car expr)))
           (cons (car expr) (map simplify (cdr expr))))

          ;; 4. If it's none of the above (e.g., a list not starting with a symbol like '(1 2 3),
          ;;    or some other data type not handled), return as is.
          (else expr))))

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

