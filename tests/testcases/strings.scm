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

;; --- string-join ---

(define-test "string-join-empty-list"
  (assert-equal (string-join '() ",") ""))

(define-test "string-join-single-item"
  (assert-equal (string-join '("hello") ",") "hello"))

(define-test "string-join-ascii"
  (assert-equal (string-join '("a" "b" "c") "-") "a-b-c"))

(define-test "string-join-empty-delimiter"
  (assert-equal (string-join '("a" "b" "c") "") "abc"))

(define-test "string-join-multi-char-delimiter"
  (assert-equal (string-join '("a" "b" "c") ", ") "a, b, c"))

(define-test "string-join-with-empty-strings"
  (assert-equal (string-join '("a" "" "c" "" "e") "-") "a--c--e"))

(define-test "string-join-utf8"
  (assert-equal (string-join '("你好" "世界") " ") "你好 世界"))

(define-test "string-join-utf8-delimiter"
  (assert-equal (string-join '("a" "b") "€") "a€b"))

;; --- string-split ---

(define-test "string-split-empty"
  (assert-equal (string-split "" #\ ) '("")) ) ; Splitting "" gives ("")

(define-test "string-split-no-delimiter"
  (assert-equal (string-split "abc" #\-) '("abc")))

(define-test "string-split-ascii"
  (assert-equal (string-split "a-b-c" #\-) '("a" "b" "c")))

(define-test "string-split-leading-delimiter"
  (assert-equal (string-split "-a-b-c" #\-) '("" "a" "b" "c")))

(define-test "string-split-trailing-delimiter"
  (assert-equal (string-split "a-b-c-" #\-) '("a" "b" "c" "")))

(define-test "string-split-consecutive-delimiters"
  (assert-equal (string-split "a--b-c" #\-) '("a" "" "b" "c")))

(define-test "string-split-all-delimiters"
  (assert-equal (string-split "---" #\-) '("" "" "" "")))

(define-test "string-split-utf8-delimiter"
  (assert-equal (string-split "你好 世界" #\ ) '("你好" "世界")))

(define-test "string-split-utf8-content"
  (assert-equal (string-split "你好-世界" #\-) '("你好" "世界")))

(define-test "string-split-utf8-delimiter-char"
  (assert-equal (string-split "a€b€c" #\€) '("a" "b" "c")))

;; --- string-tokenize ---

(define-test "string-tokenize-empty"
  (assert-equal (string-tokenize "" " ") '())) ; Tokenizing "" gives ()

(define-test "string-tokenize-no-delimiter"
  (assert-equal (string-tokenize "abc" "-") '("abc")))

(define-test "string-tokenize-ascii-single-delim"
  (assert-equal (string-tokenize "a-b-c" "-") '("a" "b" "c")))

(define-test "string-tokenize-ascii-set-delim"
  (assert-equal (string-tokenize "a-b,c d" "-, ") '("a" "b" "c" "d")))

(define-test "string-tokenize-leading-delimiters"
  (assert-equal (string-tokenize "  a b" " ") '("a" "b")))

(define-test "string-tokenize-trailing-delimiters"
  (assert-equal (string-tokenize "a b  " " ") '("a" "b")))

(define-test "string-tokenize-consecutive-delimiters"
  (assert-equal (string-tokenize "a - ,,b" " -,") '("a" "b")))

(define-test "string-tokenize-all-delimiters"
  (assert-equal (string-tokenize "-, ,-" " -,") '()))

(define-test "string-tokenize-utf8-delimiter-set"
  (assert-equal (string-tokenize "你好 世界！再见" " ！") '("你好" "世界" "再见")))

(define-test "string-tokenize-utf8-content"
  (assert-equal (string-tokenize "你好-世界,再见" "-,") '("你好" "世界" "再见")))

(define-test "string-tokenize-mixed-delimiters"
  (assert-equal (string-tokenize "a€b c,d" " €,") '("a" "b" "c" "d")))

;; --- number->string ---

(define-test "number->string-zero"
  (assert-equal (number->string 0) "0"))

(define-test "number->string-positive"
  (assert-equal (number->string 12345) "12345"))

(define-test "number->string-negative"
  (assert-equal (number->string -987) "-987"))

(define-test "number->string-large"
  (assert-equal (number->string 12345678901234567890) "12345678901234567890"))

(define-test "number->string-radix-2"
  (assert-equal (number->string 10 2) "1010"))

(define-test "number->string-radix-16"
  (assert-equal (number->string 255 16) "ff"))

(define-test "number->string-radix-16-neg"
  (assert-equal (number->string -26 16) "-1a"))

(define-test "number->string-radix-36"
  (assert-equal (number->string 12345 36) "9ix"))

;; Rational tests (radix must be 10)
(define-test "number->string-rational-simple"
  (assert-equal (number->string (/ 1 2)) "1/2"))

(define-test "number->string-rational-negative"
  (assert-equal (number->string (/ -3 4)) "-3/4"))

(define-test "number->string-rational-improper"
  (assert-equal (number->string (/ 7 3)) "7/3"))

(define-test "number->string-rational-simplified-int"
  (assert-equal (number->string (/ 10 2)) "5")) ; Should simplify to integer

(define-test "number->string-rational-simplified-neg"
  (assert-equal (number->string (/ 6 -3)) "-2")) ; Should simplify

;; --- string->number ---

(define-test "string->number-zero"
  (assert-equal (string->number "0") 0))

(define-test "string->number-positive"
  (assert-equal (string->number "12345") 12345))

(define-test "string->number-negative"
  (assert-equal (string->number "-987") -987))

(define-test "string->number-large"
  (assert-equal (string->number "12345678901234567890") 12345678901234567890))

(define-test "string->number-leading-plus"
  (assert-equal (string->number "+123") 123))

(define-test "string->number-leading-whitespace"
  (assert-equal (string->number "  123") 123))

(define-test "string->number-trailing-whitespace"
  (assert-equal (string->number "123  ") 123)) ; Change from standard! Normally disallowed trailing non-digits, but for us that's useless

(define-test "string->number-whitespace-sign"
  (assert-equal (string->number " -123 ") -123)) ; Change from standard! Normally disallowed trailing non-digits

(define-test "string->number-radix-2"
  (assert-equal (string->number "1010" 2) 10))

(define-test "string->number-radix-16"
  (assert-equal (string->number "ff" 16) 255))

(define-test "string->number-radix-16-caps"
  (assert-equal (string->number "FF" 16) 255))

(define-test "string->number-radix-16-neg"
  (assert-equal (string->number "-1a" 16) -26))

(define-test "string->number-radix-36"
  (assert-equal (string->number "9ix" 36) 12345))

(define-test "string->number-invalid-empty"
  (assert-equal (string->number "") #f))

(define-test "string->number-invalid-whitespace"
  (assert-equal (string->number "   ") #f))

(define-test "string->number-invalid-chars"
  (assert-equal (string->number "12a3") #f)) ; 'a' invalid in radix 10

(define-test "string->number-invalid-radix"
  (assert-equal (string->number "129" 8) #f)) ; '9' invalid in radix 8

(define-test "string->number-invalid-chars-radix16"
  (assert-equal (string->number "12fg" 16) #f)) ; 'g' invalid in radix 16

(define-test "string->number-just-sign"
  (assert-equal (string->number "-") #f))

(define-test "string->number-sign-whitespace"
  (assert-equal (string->number "+ ") #f))

;; Tests for non-integer formats (should return #f for now)
(define-test "string->number-fraction-string"
  (assert-equal (string->number "1/2") 1/2))  ; Extension to standard to allow fractions

;(define-test "string->number-decimal-string"
;  (assert-equal (string->number "123.45") 123.45)) ; Extension to standard to allow decimals

(define-test "string->number-exponent-string"
  (assert-equal (string->number "1e3") 1000)) ; Extension to standard to allow exponent notation

;; --- string->number Extended Tests (Fractions, Decimals, Exponents) ---

;; Fractions
(define-test "string->number-fraction-neg-num"
  (assert-equal (string->number "-1/2") -1/2))

(define-test "string->number-fraction-neg-den"
  (assert-equal (string->number "1/-2") -1/2)) ; Should canonicalize

(define-test "string->number-fraction-both-neg"
  (assert-equal (string->number "-1/-2") 1/2)) ; Should canonicalize

(define-test "string->number-fraction-whitespace"
  (assert-equal (string->number " 3 / 4 ") #f)) ; Whitespace around / are a bad idea

(define-test "string->number-fraction-non-canonical"
  (assert-equal (string->number "4/8") 1/2)) ; Should simplify

(define-test "string->number-fraction-zero-num"
  (assert-equal (string->number "0/5") 0))

(define-test "string->number-fraction-invalid-zero-den"
  (assert-equal (string->number "1/0") #f)) ; Division by zero is invalid input

(define-test "string->number-fraction-invalid-chars"
  (assert-equal (string->number "1/a") #f))

(define-test "string->number-fraction-radix-16"
  (assert-equal (string->number "a/b" 16) #f)) ; Fractions only supported for radix 10

;; Decimals
(define-test "string->number-decimal-leading-dot"
  (assert-equal (string->number ".5") 1/2))

(define-test "string->number-decimal-trailing-dot"
  (assert-equal (string->number "5.") 5))

(define-test "string->number-decimal-neg"
  (assert-equal (string->number "-12.34") -1234/100))

(define-test "string->number-decimal-zero"
  (assert-equal (string->number "0.0") 0))

(define-test "string->number-decimal-just-dot"
  (assert-equal (string->number ".") #f))

(define-test "string->number-decimal-multiple-dots"
  (assert-equal (string->number "1.2.3") #f))

(define-test "string->number-decimal-radix-16"
  (assert-equal (string->number "a.b" 16) #f)) ; Decimals only supported for radix 10

;; Scientific Notation
(define-test "string->number-exponent-positive"
  (assert-equal (string->number "1.23e3") 1230))

(define-test "string->number-exponent-neg-exp"
  (assert-equal (string->number "123e-2") 123/100))

(define-test "string->number-exponent-pos-exp-sign"
  (assert-equal (string->number "1.2e+2") 120))

(define-test "string->number-exponent-neg-significand"
  (assert-equal (string->number "-1.23e3") -1230))

(define-test "string->number-exponent-neg-significand-neg-exp"
  (assert-equal (string->number "-123e-2") -123/100))

(define-test "string->number-exponent-zero-exp"
  (assert-equal (string->number "123e0") 123))

(define-test "string->number-exponent-large-exp"
  ;; 1.2 * 10^25 = 12 * 10^24
  (assert-equal (string->number "1.2e25") 12000000000000000000000000))

(define-test "string->number-exponent-large-neg-exp"
  ;; 12 / 10^20
  (assert-equal (string->number "12e-20") 12/100000000000000000000))

(define-test "string->number-exponent-case-E"
  (assert-equal (string->number "1.5E2") 150))

(define-test "string->number-exponent-whitespace"
  (assert-equal (string->number " 1.2 e -3 ") #f)) ; Whitespace around e is a bad idea

(define-test "string->number-exponent-invalid-missing-exp-digits"
  (assert-equal (string->number "1e") #f))

(define-test "string->number-exponent-invalid-missing-exp-digits-after-sign"
  (assert-equal (string->number "1e+") #f))

(define-test "string->number-exponent-invalid-missing-significand"
  (assert-equal (string->number "e3") #f))

(define-test "string->number-exponent-invalid-dot-significand"
  (assert-equal (string->number ".e3") #f))

(define-test "string->number-radix-16-not-exponent"
  (assert-equal (string->number "1e3" 16) 483)) ; Not an exponent in base 16, 0x1e3 = 483

;; --- String Comparisons ---

(define-test "string=?-equal"
  (assert-true (string=? "hello" "hello")))

(define-test "string=?-unequal-len"
  (assert-false (string=? "hello" "hell")))

(define-test "string=?-unequal-case"
  (assert-false (string=? "hello" "Hello"))) ; Case-sensitive

(define-test "string=?-unequal-content"
  (assert-false (string=? "hello" "hellp")))

(define-test "string=?-empty"
  (assert-true (string=? "" "")))

(define-test "string=?-utf8-equal"
  (assert-true (string=? "你好" "你好")))

(define-test "string=?-utf8-unequal"
  (assert-false (string=? "你好" "你好吗")))

(define-test "string<?-less"
  (assert-true (string<? "apple" "banana")))

(define-test "string<?-equal"
  (assert-false (string<? "hello" "hello")))

(define-test "string<?-greater"
  (assert-false (string<? "zebra" "yak")))

(define-test "string<?-case"
  (assert-true (string<? "Apple" "apple"))) ; Uppercase usually comes first

(define-test "string<?-prefix"
  (assert-true (string<? "app" "apple")))

(define-test "string<?-utf8"
  (assert-false (string<? "你好" "世界"))) ; Changed to assert-false (byte order)

(define-test "string>?-greater"
  (assert-true (string>? "zebra" "yak")))

(define-test "string>?-equal"
  (assert-false (string>? "hello" "hello")))

(define-test "string>?-less"
  (assert-false (string>? "apple" "banana")))

(define-test "string<=?-less"
  (assert-true (string<=? "apple" "banana")))

(define-test "string<=?-equal"
  (assert-true (string<=? "hello" "hello")))

(define-test "string<=?-greater"
  (assert-false (string<=? "zebra" "yak")))

(define-test "string>=?-greater"
  (assert-true (string>=? "zebra" "yak")))

(define-test "string>=?-equal"
  (assert-true (string>=? "hello" "hello")))

(define-test "string>=?-less"
  (assert-false (string>=? "apple" "banana")))

;;; --- END OF STRING TESTS ---