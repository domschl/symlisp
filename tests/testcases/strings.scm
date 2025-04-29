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


(define-test "string-append-none"
  (assert-equal (string-append) ""))

(define-test "string-append-one-empty"
  (assert-equal (string-append "") ""))

(define-test "string-append-one"
  (assert-equal (string-append "hello") "hello"))

(define-test "string-append-two-ascii"
  (assert-equal (string-append "hello" " world") "hello world"))

(define-test "string-append-multiple-ascii"
  (assert-equal (string-append "a" "b" "c" "d") "abcd"))

(define-test "string-append-with-empty"
  (assert-equal (string-append "a" "" "b" "" "" "c") "abc"))

(define-test "string-append-utf8"
  (assert-equal (string-append "你好" "世界") "你好世界")) ; Hello World in Chinese

(define-test "string-append-mixed-utf8"
  (assert-equal (string-append "Euro: €" ", Gothic: 𐍈") "Euro: €, Gothic: 𐍈"))

;; --- substring ---

(define-test "substring-ascii-full"
  (assert-equal (substring "abcde" 0 5) "abcde"))

(define-test "substring-ascii-start"
  (assert-equal (substring "abcde" 0 2) "ab"))

(define-test "substring-ascii-middle"
  (assert-equal (substring "abcde" 1 4) "bcd"))

(define-test "substring-ascii-end"
  (assert-equal (substring "abcde" 3 5) "de"))

(define-test "substring-ascii-implicit-end"
  (assert-equal (substring "abcde" 2) "cde"))

(define-test "substring-ascii-single-char"
  (assert-equal (substring "abcde" 2 3) "c"))

(define-test "substring-ascii-empty-result"
  (assert-equal (substring "abcde" 2 2) ""))

(define-test "substring-empty-string"
  (assert-equal (substring "" 0 0) ""))

(define-test "substring-utf8-full"
  (assert-equal (substring "你好世界" 0 4) "你好世界"))

(define-test "substring-utf8-start"
  (assert-equal (substring "你好世界" 0 2) "你好"))

(define-test "substring-utf8-middle"
  (assert-equal (substring "你好世界" 1 3) "好世"))

(define-test "substring-utf8-end"
  (assert-equal (substring "你好世界" 2 4) "世界"))

(define-test "substring-utf8-implicit-end"
  (assert-equal (substring "你好世界" 1) "好世界"))

(define-test "substring-utf8-single-multi-byte"
  (assert-equal (substring "你好世界" 1 2) "好"))

(define-test "substring-mixed-1"
  (assert-equal (substring "a¬é𐍈!" 0 3) "a¬é"))

(define-test "substring-mixed-2"
  (assert-equal (substring "a¬é𐍈!" 1 4) "¬é𐍈"))

(define-test "substring-mixed-3"
  (assert-equal (substring "a¬é𐍈!" 3) "𐍈!"))

(define-test "substring-mixed-4-emoji"
  (assert-equal (substring "Hi😊!" 2 3) "😊"))

;; --- string->list ---

(define-test "string->list-empty"
  (assert-equal (string->list "") '()))

(define-test "string->list-ascii"
  (assert-equal (string->list "abc") '(#\a #\b #\c)))

(define-test "string->list-utf8"
  (assert-equal (string->list "你好") '(#\你 #\好)))

(define-test "string->list-mixed"
  (assert-equal (string->list "a¬€😊") '(#\a #\¬ #\€ #\😊)))

;; --- list->string ---

(define-test "list->string-empty"
  (assert-equal (list->string '()) ""))

(define-test "list->string-ascii"
  (assert-equal (list->string '(#\h #\e #\l #\l #\o)) "hello"))

(define-test "list->string-utf8"
  (assert-equal (list->string '(#\世 #\界)) "世界"))

(define-test "list->string-mixed"
  (assert-equal (list->string '(#\a #\¬ #\€ #\😊 #\!)) "a¬€😊!"))


;;; --- END OF STRING TESTS ---