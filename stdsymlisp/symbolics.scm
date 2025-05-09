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
  (let collect-and-flatten-terms ((ops (operands expr)) (accumulated-terms '()))
    (if (null? ops)
        (let* ((processed-terms (filter (lambda (term) (not (and (constant? term) (zero? term)))) accumulated-terms))
               (constants (filter constant? processed-terms))
               (non-constants (filter (lambda (x) (not (constant? x))) processed-terms))
               (sum-const (if (null? constants) 0 (apply + constants)))
               ;; Sort non-constants. This helps group terms like x and (* c x) somewhat,
               ;; but collect-like-terms does the actual matching.
               (sorted-non-constants (list-sort term<? non-constants))
               (collected-terms (collect-like-terms sorted-non-constants))
               ;; Filter out any terms that became zero after collection (e.g. x + (- x))
               (final-non-zero-terms (filter (lambda (term) (not (and (constant? term) (zero? term)))) collected-terms))
               ;; Sort the final collected terms again for canonical order
               (final-sorted-terms (list-sort term<? final-non-zero-terms)))
          (cond
            ((null? final-sorted-terms) sum-const) ; Only constants remained, or all cancelled to 0
            ((and (not (null? final-sorted-terms)) (zero? sum-const)) ; No constant part, or it's 0
             (make-sum final-sorted-terms))
            (else ; Both constant part (non-zero) and other terms
             (make-sum (cons sum-const final-sorted-terms)))))
        ;; Process next operand
        (let ((simplified-op (simplify (car ops))))
          (if (sum? simplified-op) ; Flatten sum
              (collect-and-flatten-terms (cdr ops) (append accumulated-terms (operands simplified-op)))
              (collect-and-flatten-terms (cdr ops) (append accumulated-terms (list simplified-op))))))))

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
(define (group-factors sorted-factors)
  (if (null? sorted-factors)
      '()
      (let* ((current-base-candidate (car sorted-factors))
             (current-base (if (power? current-base-candidate)
                               (base current-base-candidate)
                               current-base-candidate)))
        ;; Inner recursive helper to process all factors for the current_base
        (let collect-for-this-base ((factors-for-this-base-sequence sorted-factors) ; The sublist starting with current_base
                                     (accumulated-exponent-for-this-base 0))     ; Start sum with numeric 0
          (if (or (null? factors-for-this-base-sequence)
                  ;; Check if the next factor still belongs to the current_base
                  (let ((next-factor-base-candidate (car factors-for-this-base-sequence)))
                    (not (equal? (if (power? next-factor-base-candidate)
                                     (base next-factor-base-candidate)
                                     next-factor-base-candidate)
                                 current-base))))
              ;; This base's sequence is done, or the list of factors for this base is exhausted.
              ;; Construct the term for current_base.
              (let* (;; Simplify the accumulated exponent expression itself
                     (final-exponent (simplify accumulated-exponent-for-this-base))
                     (grouped-term
                      (cond
                        ;; If exponent simplifies to 0, factor is 1 (e.g. x^0 = 1)
                        ((and (number? final-exponent) (zero? final-exponent)) 1)
                        ;; If exponent simplifies to 1, factor is the base itself (e.g. x^1 = x)
                        ((and (number? final-exponent) (= final-exponent 1)) current-base)
                        ;; Otherwise, it's (^ base exponent)
                        (else (list '^ current-base final-exponent)))))
                ;; Prepend this grouped_term to the result of grouping the *rest* of the original list.
                ;; 'factors-for-this-base-sequence' now points to the start of the next base's sequence,
                ;; or is '() if all factors have been processed.
                (cons grouped-term (group-factors factors-for-this-base-sequence)))

              ;; Current factor (car factors-for-this-base-sequence) matches current_base.
              ;; Accumulate its exponent.
              (let* ((factor-to-process (car factors-for-this-base-sequence))
                     (exponent-of-this-factor (if (power? factor-to-process)
                                                  (exponent factor-to-process)
                                                  1))) ; Plain factor has exponent 1
                (collect-for-this-base (cdr factors-for-this-base-sequence)
                                       (add-exponents accumulated-exponent-for-this-base exponent-of-this-factor))))))))

;; Simplification for products (with flattening, ordering, and power-grouping)
;; Simplification for products (with flattening, ordering, and power-grouping)
(define (simplify-product expr)
  (let collect-and-flatten-factors ((ops (operands expr)) (accumulated-factors '()))
    (if (null? ops)
        ;; All operands processed and flattened, now process the collected factors
        (cond
          ;; If any factor was simplified to 0 earlier or is present now
          ((exists (lambda (f) (and (constant? f) (zero? f))) accumulated-factors) 0)
          (else
           (let* ((factors-no-ones (filter (lambda (f) (not (and (constant? f) (= f 1)))) accumulated-factors))
                  (constants (filter constant? factors-no-ones))
                  (non-constants (filter (lambda (x) (not (constant? x))) factors-no-ones))
                  (prod-const (if (null? constants) 1 (apply * constants)))
                  ;; term<? ensures that 'a' and '(^ a n)' might not be adjacent if other vars are between.
                  ;; group-factors needs to handle this by processing one base at a time from the sorted list.
                  (sorted-non-constants (list-sort term<? non-constants))
                  (grouped-non-constants (group-factors sorted-non-constants))
                  ;; Filter out any '1's that might have resulted from x^0
                  (final-grouped-factors-no-ones (filter (lambda (f) (not (and (constant? f) (= f 1)))) grouped-non-constants))
                  ;; Sort the final grouped non-constant factors again for canonical order
                  (final-sorted-grouped-factors (list-sort term<? final-grouped-factors-no-ones)))
             (cond
               ((zero? prod-const) 0)
               ((null? final-sorted-grouped-factors) prod-const)
               ((= prod-const 1)
                (make-product final-sorted-grouped-factors))
               ((= prod-const -1)
                (simplify (make-negation (make-product final-sorted-grouped-factors))))
               (else
                (make-product (cons prod-const final-sorted-grouped-factors)))))))
        ;; Process next operand
        (let ((simplified-op (simplify (car ops))))
          (if (and (constant? simplified-op) (zero? simplified-op))
              0
              (if (product? simplified-op)
                  (collect-and-flatten-factors (cdr ops) (append accumulated-factors (operands simplified-op)))
                  (collect-and-flatten-factors (cdr ops) (append accumulated-factors (list simplified-op)))))))))


;; Simplification for powers
(define (simplify-power expr)
  (let ((b (simplify (base expr)))
        (e (simplify (exponent expr))))
    (cond
      ;; e is 0 => x^0 = 1 (assuming b != 0, common simplification)
      ((and (constant? e) (zero? e)) 1)
      ;; e is 1 => x^1 = x
      ((and (constant? e) (= e 1)) b)
      ;; b is 0 (and e is not 0) => 0^x = 0 (for x > 0)
      ((and (constant? b) (zero? b) (not (and (constant? e) (zero? e)))) 0)
      ;; b is 1 => 1^x = 1
      ((and (constant? b) (= b 1)) 1)
      ;; Both b and e are constants
      ((and (constant? b) (constant? e)) (expt b e))
      ;; Default case
      (else (list '^ b e)))))

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

