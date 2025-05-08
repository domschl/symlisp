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

;; (memq obj list)
;; Searches list for an occurrence of obj using eq?.
;; If obj is found, returns the sublist of list starting with the first occurrence of obj.
;; Otherwise, returns #f.
(define (memq obj lst)
  (cond
    ((null? lst) #f) ; Base case: obj not found
    ((eq? obj (car lst)) lst) ; Found: return the sublist starting with obj
    (else (memq obj (cdr lst))))) ; Recurse on the rest of the list

(define (exists pred lst)
  (cond
    ((null? lst) #f)
    ((pred (car lst)) (pred (car lst))) ; Return the true value from pred
    (else (exists pred (cdr lst)))))

;; (list-take lst k)
;; Returns a new list containing the first k elements of lst.
;; If k is 0, returns '().
;; If k is >= (length lst), returns a copy of lst.
(define (list-take lst k)
  (if (or (<= k 0) (null? lst))
      '()
      (cons (car lst) (list-take (cdr lst) (- k 1)))))

;; (list-drop lst k)
;; Returns the sublist of lst after dropping the first k elements.
;; If k is 0, returns lst.
;; If k is >= (length lst), returns '().
(define (list-drop lst k)
  (if (or (<= k 0) (null? lst))
      lst
      (list-drop (cdr lst) (- k 1))))

;; (merge-sorted-lists list1 list2 pred)
;; Merges two lists, list1 and list2, that are already sorted
;; according to the predicate pred. Pred should return #t if its
;; first argument is "less than" its second.
(define (merge-sorted-lists list1 list2 pred)
  (cond
    ((null? list1) list2)
    ((null? list2) list1)
    ((pred (car list1) (car list2))
     (cons (car list1) (merge-sorted-lists (cdr list1) list2 pred)))
    (else
     (cons (car list2) (merge-sorted-lists list1 (cdr list2) pred)))))

;; (list-sort pred lst)
;; Returns a new list containing the elements of lst, sorted
;; according to the predicate pred. This is a non-destructive
;; merge sort. Pred should return #t if its first argument is
;; "less than" its second.
(define (list-sort pred lst)
  (if (or (null? lst) (null? (cdr lst))) ; Base case: 0 or 1 element
      lst
      (let* ((len (length lst))
             (mid (quotient len 2))
             (left-half (list-take lst mid))
             (right-half (list-drop lst mid)))
        (merge-sorted-lists
         (list-sort pred left-half)
         (list-sort pred right-half)
         pred))))


;;;-----------------------------------------------------------------------------
;;; Numeric Predicates (can be moved to a numbers.scm later)
;;;-----------------------------------------------------------------------------

;; (zero? n)
;; Returns #t if n is the number zero, #f otherwise.
(define (zero? n)
  (and (number? n) (= n 0)))


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
