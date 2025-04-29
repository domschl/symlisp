;; Tests for String Functions

;; --- string-length ---

(define-test "string-length-empty"
  (assert-equal (string-length "") 0))

(define-test "string-length-ascii"
  (assert-equal (string-length "abc") 3))

(define-test "string-length-utf8-2byte"
  (assert-equal (string-length "¬£") 2)) ; Each is 2 bytes

(define-test "string-length-utf8-3byte"
  (assert-equal (string-length "é€") 2)) ; Each is 3 bytes (Euro sign)

(define-test "string-length-utf8-4byte"
  (assert-equal (string-length "𐍈😊") 2)) ; Each is 4 bytes (Gothic Hwair, Smiling Face)

(define-test "string-length-mixed"
  (assert-equal (string-length "a¬é𐍈!") 5)) ; 1, 2, 3, 4, 1 bytes

(define-test "string-length-invalid-utf8"
  ;; Assuming decode_utf8 replaces invalid sequences with U+FFFD
  ;; and string-length counts these replacement characters.
  ;; \xC3 without following byte
  ;; \xE2\x82 without final byte
  ;; \xF0\x90\x8D without final byte
  (assert-equal (string-length "abc\xC3def\xE2\x82ghi\xF0\x90\x8Djkl") 18))
  
;; --- string-ref ---

(define-test "string-ref-ascii-start"
  (assert-equal (string-ref "abc" 0) #\a))

(define-test "string-ref-ascii-middle"
  (assert-equal (string-ref "abc" 1) #\b))

(define-test "string-ref-ascii-end"
  (assert-equal (string-ref "abc" 2) #\c))

(define-test "string-ref-utf8-2byte"
  (assert-equal (string-ref "¬£" 1) #\£))

(define-test "string-ref-utf8-3byte"
  (assert-equal (string-ref "café" 3) #\é))

(define-test "string-ref-utf8-4byte"
  (assert-equal (string-ref "Hi😊!" 2) #\😊))

(define-test "string-ref-mixed"
  (assert-equal (string-ref "a¬é𐍈!" 0) #\a))
(define-test "string-ref-mixed-2"
  (assert-equal (string-ref "a¬é𐍈!" 1) #\¬))
(define-test "string-ref-mixed-3"
  (assert-equal (string-ref "a¬é𐍈!" 2) #\é))
(define-test "string-ref-mixed-4"
  (assert-equal (string-ref "a¬é𐍈!" 3) #\𐍈)) ; Requires parser support for #\Uxxxx or direct UTF-8 #\
(define-test "string-ref-mixed-5"
  (assert-equal (string-ref "a¬é𐍈!" 4) #\!))


;;; --- END OF STRING TESTS ---