;;;-----------------------------------------------------------------------------
;;; List Utilities
;;;-----------------------------------------------------------------------------

;; (assoc key alist)
;; Finds the first pair in alist whose car is equal? to key.
;; Returns the pair or #f if not found.
(define (assoc key alist)
  (cond
    ((null? alist) #f) ; Base case: Not found
    ((equal? key (caar alist)) ; Check car of the first pair
     (car alist)) ; Found, return the pair
    (else (assoc key (cdr alist))))) ; Recurse on the rest of the list

;;;-----------------------------------------------------------------------------
;;; Infix Expression Parser (Shunting-Yard Algorithm to Prefix S-expression)
;;;-----------------------------------------------------------------------------
;;; This module converts a list of infix tokens (strings) produced by
;;; string->infix-tokens into a standard SymLisp prefix S-expression.
;;;-----------------------------------------------------------------------------

;; Helper for systems without native multiple value support:
;; (values v1 v2 ...) -> (list v1 v2 ...)
(define (values . args)
  args)

;; call-with-values implementation for systems where 'values' returns a list.
;; (call-with-values producer-thunk consumer-procedure)
;; producer-thunk: a procedure of 0 arguments that calls (values ...)
;; consumer-procedure: a procedure that accepts as many arguments as producer-thunk returns
(define (call-with-values producer consumer)
  (apply consumer (producer)))
