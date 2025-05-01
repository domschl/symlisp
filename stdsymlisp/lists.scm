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
