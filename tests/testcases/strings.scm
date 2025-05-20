;; Tests for String Functions

;;;-----------------------------------------------------------------------------
;;; String Constructor
;;;-----------------------------------------------------------------------------

(define-test-thunked "string-constructor-empty"
  (lambda () (assert-equal "" (string))))

(define-test-thunked "string-constructor-single-ascii"
  (lambda () (assert-equal "a" (string #\a))))

(define-test-thunked "string-constructor-multiple-ascii"
  (lambda () (assert-equal "abc" (string #\a #\b #\c))))

(define-test-thunked "string-constructor-single-unicode"
  (lambda () (assert-equal "λ" (string #\λ)))) ; U+03BB

(define-test-thunked "string-constructor-multiple-unicode"
  (lambda () (assert-equal "Γειά" (string #\Γ #\ε #\ι #\ά)))) ; U+0393 U+03B5 U+03B9 U+03AC

(define-test-thunked "string-constructor-mixed"
  (lambda () (assert-equal "Hello λ!" (string #\H #\e #\l #\l #\o #\space #\λ #\!))))

(define-test-thunked "list-and-back"
  (lambda () (assert-equal (apply string (string->list "asdf")) "asdf")))

;(define-test-thunked "string-constructor-error-type"
;  (lambda () (assert-error? (string #\a 1 #\c))))


;; --- string-length ---

(define-test-thunked "string-length-empty"
  (lambda () (assert-equal (string-length "") 0)))

(define-test-thunked "string-length-ascii"
  (lambda () (assert-equal (string-length "abc") 3)))

(define-test-thunked "string-length-utf8-2byte"
  (lambda () (assert-equal (string-length "¬£") 2))) ; Each is 2 bytes

(define-test-thunked "string-length-utf8-3byte"
  (lambda () (assert-equal (string-length "é€") 2))) ; Each is 3 bytes (Euro sign)

(define-test-thunked "string-length-utf8-4byte"
  (lambda () (assert-equal (string-length "𐍈😊") 2))) ; Each is 4 bytes (Gothic Hwair, Smiling Face)

(define-test-thunked "string-length-mixed"
  (lambda () (assert-equal (string-length "a¬é𐍈!") 5))) ; 1, 2, 3, 4, 1 bytes

(define-test-thunked "string-length-invalid-utf8"
  ;; Assuming decode_utf8 replaces invalid sequences with U+FFFD
  ;; and string-length counts these replacement characters.
  ;; \xC3 without following byte
  ;; \xE2\x82 without final byte
  ;; \xF0\x90\x8D without final byte
  (lambda () (assert-equal (string-length "abc\xC3def\xE2\x82ghi\xF0\x90\x8Djkl") 18)))
  
;; --- string-ref ---

(define-test-thunked "string-ref-ascii-start"
  (lambda () (assert-equal (string-ref "abc" 0) #\a)))

(define-test-thunked "string-ref-ascii-middle"
  (lambda () (assert-equal (string-ref "abc" 1) #\b)))

(define-test-thunked "string-ref-ascii-end"
  (lambda () (assert-equal (string-ref "abc" 2) #\c)))

(define-test-thunked "string-ref-utf8-2byte"
  (lambda () (assert-equal (string-ref "¬£" 1) #\£)))

(define-test-thunked "string-ref-utf8-3byte"
  (lambda () (assert-equal (string-ref "café" 3) #\é)))

(define-test-thunked "string-ref-utf8-4byte"
  (lambda () (assert-equal (string-ref "Hi😊!" 2) #\😊)))

(define-test-thunked "string-ref-mixed"
  (lambda () (assert-equal (string-ref "a¬é𐍈!" 0) #\a)))
(define-test-thunked "string-ref-mixed-2"
  (lambda () (assert-equal (string-ref "a¬é𐍈!" 1) #\¬)))
(define-test-thunked "string-ref-mixed-3"
  (lambda () (assert-equal (string-ref "a¬é𐍈!" 2) #\é)))
(define-test-thunked "string-ref-mixed-4"
  (lambda () (assert-equal (string-ref "a¬é𐍈!" 3) #\𐍈))) ; Requires parser support for #\Uxxxx or direct UTF-8 #\
(define-test-thunked "string-ref-mixed-5"
  (lambda () (assert-equal (string-ref "a¬é𐍈!" 4) #\!)))


(define-test-thunked "string-append-none"
  (lambda () (assert-equal (string-append) "")))

(define-test-thunked "string-append-one-empty"
  (lambda () (assert-equal (string-append "") "")))

(define-test-thunked "string-append-one"
  (lambda () (assert-equal (string-append "hello") "hello")))

(define-test-thunked "string-append-two-ascii"
  (lambda () (assert-equal (string-append "hello" " world") "hello world")))

(define-test-thunked "string-append-multiple-ascii"
  (lambda () (assert-equal (string-append "a" "b" "c" "d") "abcd")))

(define-test-thunked "string-append-with-empty"
  (lambda () (assert-equal (string-append "a" "" "b" "" "" "c") "abc")))

(define-test-thunked "string-append-utf8"
  (lambda () (assert-equal (string-append "你好" "世界") "你好世界"))) ; Hello World in Chinese

(define-test-thunked "string-append-mixed-utf8"
  (lambda () (assert-equal (string-append "Euro: €" ", Gothic: 𐍈") "Euro: €, Gothic: 𐍈")))

;; --- substring ---

(define-test-thunked "substring-ascii-full"
  (lambda () (assert-equal (substring "abcde" 0 5) "abcde")))

(define-test-thunked "substring-ascii-start"
  (lambda () (assert-equal (substring "abcde" 0 2) "ab")))

(define-test-thunked "substring-ascii-middle"
  (lambda () (assert-equal (substring "abcde" 1 4) "bcd")))

(define-test-thunked "substring-ascii-end"
  (lambda () (assert-equal (substring "abcde" 3 5) "de")))

(define-test-thunked "substring-ascii-implicit-end"
  (lambda () (assert-equal (substring "abcde" 2) "cde")))

(define-test-thunked "substring-ascii-single-char"
  (lambda () (assert-equal (substring "abcde" 2 3) "c")))

(define-test-thunked "substring-ascii-empty-result"
  (lambda () (assert-equal (substring "abcde" 2 2) "")))

(define-test-thunked "substring-empty-string"
  (lambda () (assert-equal (substring "" 0 0) "")))

(define-test-thunked "substring-utf8-full"
  (lambda () (assert-equal (substring "你好世界" 0 4) "你好世界")))

(define-test-thunked "substring-utf8-start"
  (lambda () (assert-equal (substring "你好世界" 0 2) "你好")))

(define-test-thunked "substring-utf8-middle"
  (lambda () (assert-equal (substring "你好世界" 1 3) "好世")))

(define-test-thunked "substring-utf8-end"
  (lambda () (assert-equal (substring "你好世界" 2 4) "世界")))

(define-test-thunked "substring-utf8-implicit-end"
  (lambda () (assert-equal (substring "你好世界" 1) "好世界")))

(define-test-thunked "substring-utf8-single-multi-byte"
  (lambda () (assert-equal (substring "你好世界" 1 2) "好")))

(define-test-thunked "substring-mixed-1"
  (lambda () (assert-equal (substring "a¬é𐍈!" 0 3) "a¬é")))

(define-test-thunked "substring-mixed-2"
  (lambda () (assert-equal (substring "a¬é𐍈!" 1 4) "¬é𐍈")))

(define-test-thunked "substring-mixed-3"
  (lambda () (assert-equal (substring "a¬é𐍈!" 3) "𐍈!")))

(define-test-thunked "substring-mixed-4-emoji"
  (lambda () (assert-equal (substring "Hi😊!" 2 3) "😊")))

;; --- string->list ---

(define-test-thunked "string->list-empty"
  (lambda () (assert-equal (string->list "") '())))

(define-test-thunked "string->list-ascii"
  (lambda () (assert-equal (string->list "abc") '(#\a #\b #\c))))

(define-test-thunked "string->list-utf8"
  (lambda () (assert-equal (string->list "你好") '(#\你 #\好))))

(define-test-thunked "string->list-mixed"
  (lambda () (assert-equal (string->list "a¬€😊") '(#\a #\¬ #\€ #\😊))))

;; --- list->string ---

(define-test-thunked "list->string-empty"
  (lambda () (assert-equal (list->string '()) "")))

(define-test-thunked "list->string-ascii"
  (lambda () (assert-equal (list->string '(#\h #\e #\l #\l #\o)) "hello")))

(define-test-thunked "list->string-utf8"
  (lambda () (assert-equal (list->string '(#\世 #\界)) "世界")))

(define-test-thunked "list->string-mixed"
  (lambda () (assert-equal (list->string '(#\a #\¬ #\€ #\😊 #\!)) "a¬€😊!")))

;; --- string-join ---

(define-test-thunked "string-join-empty-list"
  (lambda () (assert-equal (string-join '() ",") "")))

(define-test-thunked "string-join-single-item"
  (lambda () (assert-equal (string-join '("hello") ",") "hello")))

(define-test-thunked "string-join-ascii"
  (lambda () (assert-equal (string-join '("a" "b" "c") "-") "a-b-c")))

(define-test-thunked "string-join-empty-delimiter"
  (lambda () (assert-equal (string-join '("a" "b" "c") "") "abc")))

(define-test-thunked "string-join-multi-char-delimiter"
  (lambda () (assert-equal (string-join '("a" "b" "c") ", ") "a, b, c")))

(define-test-thunked "string-join-with-empty-strings"
  (lambda () (assert-equal (string-join '("a" "" "c" "" "e") "-") "a--c--e")))

(define-test-thunked "string-join-utf8"
  (lambda () (assert-equal (string-join '("你好" "世界") " ") "你好 世界")))

(define-test-thunked "string-join-utf8-delimiter"
  (lambda () (assert-equal (string-join '("a" "b") "€") "a€b")))

;; --- string-split ---

(define-test-thunked "string-split-empty"
  (lambda () (assert-equal (string-split "" #\ ) '("")) )) ; Splitting "" gives ("")

(define-test-thunked "string-split-no-delimiter"
  (lambda () (assert-equal (string-split "abc" #\-) '("abc"))))

(define-test-thunked "string-split-ascii"
  (lambda () (assert-equal (string-split "a-b-c" #\-) '("a" "b" "c"))))

(define-test-thunked "string-split-leading-delimiter"
  (lambda () (assert-equal (string-split "-a-b-c" #\-) '("" "a" "b" "c"))))

(define-test-thunked "string-split-trailing-delimiter"
  (lambda () (assert-equal (string-split "a-b-c-" #\-) '("a" "b" "c" ""))))

(define-test-thunked "string-split-consecutive-delimiters"
  (lambda () (assert-equal (string-split "a--b-c" #\-) '("a" "" "b" "c"))))

(define-test-thunked "string-split-all-delimiters"
  (lambda () (assert-equal (string-split "---" #\-) '("" "" "" ""))))

(define-test-thunked "string-split-utf8-delimiter"
  (lambda () (assert-equal (string-split "你好 世界" #\ ) '("你好" "世界"))))

(define-test-thunked "string-split-utf8-content"
  (lambda () (assert-equal (string-split "你好-世界" #\-) '("你好" "世界"))))

(define-test-thunked "string-split-utf8-delimiter-char"
  (lambda () (assert-equal (string-split "a€b€c" #\€) '("a" "b" "c"))))

;; --- string-tokenize ---

(define-test-thunked "string-tokenize-empty"
  (lambda () (assert-equal (string-tokenize "" " ") '()))) ; Tokenizing "" gives ()

(define-test-thunked "string-tokenize-no-delimiter"
  (lambda () (assert-equal (string-tokenize "abc" "-") '("abc"))))

(define-test-thunked "string-tokenize-ascii-single-delim"
  (lambda () (assert-equal (string-tokenize "a-b-c" "-") '("a" "b" "c"))))

(define-test-thunked "string-tokenize-ascii-set-delim"
  (lambda () (assert-equal (string-tokenize "a-b,c d" "-, ") '("a" "b" "c" "d"))))

(define-test-thunked "string-tokenize-leading-delimiters"
  (lambda () (assert-equal (string-tokenize "  a b" " ") '("a" "b"))))

(define-test-thunked "string-tokenize-trailing-delimiters"
  (lambda () (assert-equal (string-tokenize "a b  " " ") '("a" "b"))))

(define-test-thunked "string-tokenize-consecutive-delimiters"
  (lambda () (assert-equal (string-tokenize "a - ,,b" " -,") '("a" "b"))))

(define-test-thunked "string-tokenize-all-delimiters"
  (lambda () (assert-equal (string-tokenize "-, ,-" " -,") '())))

(define-test-thunked "string-tokenize-utf8-delimiter-set"
  (lambda () (assert-equal (string-tokenize "你好 世界！再见" " ！") '("你好" "世界" "再见"))))

(define-test-thunked "string-tokenize-utf8-content"
  (lambda () (assert-equal (string-tokenize "你好-世界,再见" "-,") '("你好" "世界" "再见"))))

(define-test-thunked "string-tokenize-mixed-delimiters"
  (lambda () (assert-equal (string-tokenize "a€b c,d" " €,") '("a" "b" "c" "d"))))

;; --- number->string ---

(define-test-thunked "number->string-zero"
  (lambda () (assert-equal (number->string 0) "0")))

(define-test-thunked "number->string-positive"
  (lambda () (assert-equal (number->string 12345) "12345")))

(define-test-thunked "number->string-negative"
  (lambda () (assert-equal (number->string -987) "-987")))

(define-test-thunked "number->string-large"
  (lambda () (assert-equal (number->string 12345678901234567890) "12345678901234567890")))

(define-test-thunked "number->string-radix-2"
  (lambda () (assert-equal (number->string 10 2) "1010")))

(define-test-thunked "number->string-radix-16"
  (lambda () (assert-equal (number->string 255 16) "ff")))

(define-test-thunked "number->string-radix-16-neg"
  (lambda () (assert-equal (number->string -26 16) "-1a")))

(define-test-thunked "number->string-radix-36"
  (lambda () (assert-equal (number->string 12345 36) "9ix")))

;; Rational tests (radix must be 10)
(define-test-thunked "number->string-rational-simple"
  (lambda () (assert-equal (number->string (/ 1 2)) "1/2")))

(define-test-thunked "number->string-rational-negative"
  (lambda () (assert-equal (number->string (/ -3 4)) "-3/4")))

(define-test-thunked "number->string-rational-improper"
  (lambda () (assert-equal (number->string (/ 7 3)) "7/3")))

(define-test-thunked "number->string-rational-simplified-int"
  (lambda () (assert-equal (number->string (/ 10 2)) "5"))) ; Should simplify to integer

(define-test-thunked "number->string-rational-simplified-neg"
  (lambda () (assert-equal (number->string (/ 6 -3)) "-2"))) ; Should simplify

;; --- string->number ---

(define-test-thunked "string->number-zero"
  (lambda () (assert-equal (string->number "0") 0)))

(define-test-thunked "string->number-positive"
  (lambda () (assert-equal (string->number "12345") 12345)))

(define-test-thunked "string->number-negative"
  (lambda () (assert-equal (string->number "-987") -987)))

(define-test-thunked "string->number-large"
  (lambda () (assert-equal (string->number "12345678901234567890") 12345678901234567890)))

(define-test-thunked "string->number-leading-plus"
  (lambda () (assert-equal (string->number "+123") 123)))

(define-test-thunked "string->number-leading-whitespace"
  (lambda () (assert-equal (string->number "  123") 123)))

(define-test-thunked "string->number-trailing-whitespace"
  (lambda () (assert-equal (string->number "123  ") 123))) ; Change from standard! Normally disallowed trailing non-digits, but for us that's useless

(define-test-thunked "string->number-whitespace-sign"
  (lambda () (assert-equal (string->number " -123 ") -123))) ; Change from standard! Normally disallowed trailing non-digits

(define-test-thunked "string->number-radix-2"
  (lambda () (assert-equal (string->number "1010" 2) 10)))

(define-test-thunked "string->number-radix-16"
  (lambda () (assert-equal (string->number "ff" 16) 255)))

(define-test-thunked "string->number-radix-16-caps"
  (lambda () (assert-equal (string->number "FF" 16) 255)))

(define-test-thunked "string->number-radix-16-neg"
  (lambda () (assert-equal (string->number "-1a" 16) -26)))

(define-test-thunked "string->number-radix-36"
  (lambda () (assert-equal (string->number "9ix" 36) 12345)))

(define-test-thunked "string->number-invalid-empty"
  (lambda () (assert-equal (string->number "") #f)))

(define-test-thunked "string->number-invalid-whitespace"
  (lambda () (assert-equal (string->number "   ") #f)))

(define-test-thunked "string->number-invalid-chars"
  (lambda () (assert-equal (string->number "12a3") #f))) ; 'a' invalid in radix 10

(define-test-thunked "string->number-invalid-radix"
  (lambda () (assert-equal (string->number "129" 8) #f))) ; '9' invalid in radix 8

(define-test-thunked "string->number-invalid-chars-radix16"
  (lambda () (assert-equal (string->number "12fg" 16) #f))) ; 'g' invalid in radix 16

(define-test-thunked "string->number-just-sign"
  (lambda () (assert-equal (string->number "-") #f)))

(define-test-thunked "string->number-sign-whitespace"
  (lambda () (assert-equal (string->number "+ ") #f)))

;; Tests for non-integer formats (should return #f for now)
(define-test-thunked "string->number-fraction-string"
  (lambda () (assert-equal (string->number "1/2") 1/2)))  ; Extension to standard to allow fractions

;(define-test-thunked "string->number-decimal-string"
;  (lambda () (assert-equal (string->number "123.45") 123.45))) ; Extension to standard to allow decimals

(define-test-thunked "string->number-exponent-string"
  (lambda () (assert-equal (string->number "1e3") 1000))) ; Extension to standard to allow exponent notation

;; --- string->number Extended Tests (Fractions, Decimals, Exponents) ---

;; Fractions
(define-test-thunked "string->number-fraction-neg-num"
  (lambda () (assert-equal (string->number "-1/2") -1/2)))

(define-test-thunked "string->number-fraction-neg-den"
  (lambda () (assert-equal (string->number "1/-2") -1/2))) ; Should canonicalize

(define-test-thunked "string->number-fraction-both-neg"
  (lambda () (assert-equal (string->number "-1/-2") 1/2))) ; Should canonicalize

(define-test-thunked "string->number-fraction-whitespace"
  (lambda () (assert-equal (string->number " 3 / 4 ") #f))) ; Whitespace around / are a bad idea

(define-test-thunked "string->number-fraction-non-canonical"
  (lambda () (assert-equal (string->number "4/8") 1/2))) ; Should simplify

(define-test-thunked "string->number-fraction-zero-num"
  (lambda () (assert-equal (string->number "0/5") 0)))

(define-test-thunked "string->number-fraction-invalid-zero-den"
  (lambda () (assert-equal (string->number "1/0") #f))) ; Division by zero is invalid input

(define-test-thunked "string->number-fraction-invalid-chars"
  (lambda () (assert-equal (string->number "1/a") #f)))

(define-test-thunked "string->number-fraction-radix-16"
  (lambda () (assert-equal (string->number "a/b" 16) #f))) ; Fractions only supported for radix 10

;; Decimals
(define-test-thunked "string->number-decimal-leading-dot"
  (lambda () (assert-equal (string->number ".5") 1/2)))

(define-test-thunked "string->number-decimal-trailing-dot"
  (lambda () (assert-equal (string->number "5.") 5)))

(define-test-thunked "string->number-decimal-neg"
  (lambda () (assert-equal (string->number "-12.34") -1234/100)))

(define-test-thunked "string->number-decimal-zero"
  (lambda () (assert-equal (string->number "0.0") 0)))

(define-test-thunked "string->number-decimal-just-dot"
  (lambda () (assert-equal (string->number ".") #f)))

(define-test-thunked "string->number-decimal-multiple-dots"
  (lambda () (assert-equal (string->number "1.2.3") #f)))

(define-test-thunked "string->number-decimal-radix-16"
  (lambda () (assert-equal (string->number "a.b" 16) #f))) ; Decimals only supported for radix 10

;; Scientific Notation
(define-test-thunked "string->number-exponent-positive"
  (lambda () (assert-equal (string->number "1.23e3") 1230)))

(define-test-thunked "string->number-exponent-neg-exp"
  (lambda () (assert-equal (string->number "123e-2") 123/100)))

(define-test-thunked "string->number-exponent-pos-exp-sign"
  (lambda () (assert-equal (string->number "1.2e+2") 120)))

(define-test-thunked "string->number-exponent-neg-significand"
  (lambda () (assert-equal (string->number "-1.23e3") -1230)))

(define-test-thunked "string->number-exponent-neg-significand-neg-exp"
  (lambda () (assert-equal (string->number "-123e-2") -123/100)))

(define-test-thunked "string->number-exponent-zero-exp"
  (lambda () (assert-equal (string->number "123e0") 123)))

(define-test-thunked "string->number-exponent-large-exp"
  (lambda () ;; 1.2 * 10^25 = 12 * 10^24
    (assert-equal (string->number "1.2e25") 12000000000000000000000000)))

(define-test-thunked "string->number-exponent-large-neg-exp"
  (lambda () ;; 12 / 10^20
    (assert-equal (string->number "12e-20") 12/100000000000000000000)))

(define-test-thunked "string->number-exponent-case-E"
  (lambda () (assert-equal (string->number "1.5E2") 150)))

(define-test-thunked "string->number-exponent-whitespace"
  (lambda () (assert-equal (string->number " 1.2 e -3 ") #f))) ; Whitespace around e is a bad idea

(define-test-thunked "string->number-exponent-invalid-missing-exp-digits"
  (lambda () (assert-equal (string->number "1e") #f)))

(define-test-thunked "string->number-exponent-invalid-missing-exp-digits-after-sign"
  (lambda () (assert-equal (string->number "1e+") #f)))

(define-test-thunked "string->number-exponent-invalid-missing-significand"
  (lambda () (assert-equal (string->number "e3") #f)))

(define-test-thunked "string->number-exponent-invalid-dot-significand"
  (lambda () (assert-equal (string->number ".e3") #f)))

(define-test-thunked "string->number-radix-16-not-exponent"
  (lambda () (assert-equal (string->number "1e3" 16) 483))) ; Not an exponent in base 16, 0x1e3 = 483

;; --- String Comparisons ---

(define-test-thunked "string=?-equal"
  (lambda () (assert-true (string=? "hello" "hello"))))

(define-test-thunked "string=?-unequal-len"
  (lambda () (assert-false (string=? "hello" "hell"))))

(define-test-thunked "string=?-unequal-case"
  (lambda () (assert-false (string=? "hello" "Hello")))) ; Case-sensitive

(define-test-thunked "string=?-unequal-content"
  (lambda () (assert-false (string=? "hello" "hellp"))))

(define-test-thunked "string=?-empty"
  (lambda () (assert-true (string=? "" ""))))

(define-test-thunked "string=?-utf8-equal"
  (lambda () (assert-true (string=? "你好" "你好"))))

(define-test-thunked "string=?-utf8-unequal"
  (lambda () (assert-false (string=? "你好" "你好吗"))))

(define-test-thunked "string<?-less"
  (lambda () (assert-true (string<? "apple" "banana"))))

(define-test-thunked "string<?-equal"
  (lambda () (assert-false (string<? "hello" "hello"))))

(define-test-thunked "string<?-greater"
  (lambda () (assert-false (string<? "zebra" "yak"))))

(define-test-thunked "string<?-case"
  (lambda () (assert-true (string<? "Apple" "apple")))) ; Uppercase usually comes first

(define-test-thunked "string<?-prefix"
  (lambda () (assert-true (string<? "app" "apple"))))

(define-test-thunked "string<?-utf8"
  (lambda () (assert-false (string<? "你好" "世界")))) ; Changed to assert-false (byte order)

(define-test-thunked "string>?-greater"
  (lambda () (assert-true (string>? "zebra" "yak"))))

(define-test-thunked "string>?-equal"
  (lambda () (assert-false (string>? "hello" "hello"))))

(define-test-thunked "string>?-less"
  (lambda () (assert-false (string>? "apple" "banana"))))

(define-test-thunked "string<=?-less"
  (lambda () (assert-true (string<=? "apple" "banana"))))

(define-test-thunked "string<=?-equal"
  (lambda () (assert-true (string<=? "hello" "hello"))))

(define-test-thunked "string<=?-greater"
  (lambda () (assert-false (string<=? "zebra" "yak"))))

(define-test-thunked "string>=?-greater"
  (lambda () (assert-true (string>=? "zebra" "yak"))))

(define-test-thunked "string>=?-equal"
  (lambda () (assert-true (string>=? "hello" "hello"))))

(define-test-thunked "string>=?-less"
  (lambda () (assert-false (string>=? "apple" "banana"))))

;;;-----------------------------------------------------------------------------
;;; Character Comparison Functions
;;;-----------------------------------------------------------------------------

;;; char=? Tests
(define-test-thunked "char=?-equal-ascii"
  (lambda () (assert-true (char=? #\a #\a))))

(define-test-thunked "char=?-unequal-ascii"
  (lambda () (assert-false (char=? #\a #\b))))

(define-test-thunked "char=?-case-sensitive"
  (lambda () (assert-false (char=? #\a #\A))))

(define-test-thunked "char=?-equal-unicode"
  (lambda () (assert-true (char=? #\λ #\λ))))

(define-test-thunked "char=?-unequal-unicode"
  (lambda () (assert-false (char=? #\λ #\μ))))

(define-test-thunked "char=?-special-chars"
  (lambda () (assert-true (char=? #\space #\space))))
(define-test-thunked "char=?-special-chars-unequal"
  (lambda () (assert-false (char=? #\newline #\tab))))

;;; char<? Tests
(define-test-thunked "char<?-less-ascii"
  (lambda () (assert-true (char<? #\a #\b))))

(define-test-thunked "char<?-equal-ascii"
  (lambda () (assert-false (char<? #\a #\a))))

(define-test-thunked "char<?-greater-ascii"
  (lambda () (assert-false (char<? #\b #\a))))

(define-test-thunked "char<?-case-sensitive"
  (lambda () (assert-true (char<? #\A #\a)))) ; Uppercase typically before lowercase

(define-test-thunked "char<?-less-unicode"
  (lambda () (assert-true (char<? #\α #\β)))) ; Greek alpha, beta

(define-test-thunked "char<?-equal-unicode"
  (lambda () (assert-false (char<? #\α #\α))))

;;; char>? Tests
(define-test-thunked "char>?-greater-ascii"
  (lambda () (assert-true (char>? #\b #\a))))

(define-test-thunked "char>?-equal-ascii"
  (lambda () (assert-false (char>? #\a #\a))))

(define-test-thunked "char>?-less-ascii"
  (lambda () (assert-false (char>? #\a #\b))))

(define-test-thunked "char>?-case-sensitive"
  (lambda () (assert-false (char>? #\A #\a)))) ; Uppercase typically before lowercase

(define-test-thunked "char>?-greater-unicode"
  (lambda () (assert-true (char>? #\β #\α))))

;;; char<=? Tests
(define-test-thunked "char<=?-less-ascii"
  (lambda () (assert-true (char<=? #\a #\b))))

(define-test-thunked "char<=?-equal-ascii"
  (lambda () (assert-true (char<=? #\a #\a))))

(define-test-thunked "char<=?-greater-ascii"
  (lambda () (assert-false (char<=? #\b #\a))))

(define-test-thunked "char<=?-case-sensitive"
  (lambda () (assert-true (char<=? #\A #\a))))

;;; char>=? Tests
(define-test-thunked "char>=?-greater-ascii"
  (lambda () (assert-true (char>=? #\b #\a))))

(define-test-thunked "char>=?-equal-ascii"
  (lambda () (assert-true (char>=? #\a #\a))))

(define-test-thunked "char>=?-less-ascii"
  (lambda () (assert-false (char>=? #\a #\b))))

(define-test-thunked "char>=?-case-sensitive"
  (lambda () (assert-false (char>=? #\A #\a))))


;;;-----------------------------------------------------------------------------
;;; Character Comparison Functions (Case-Insensitive)
;;;-----------------------------------------------------------------------------

;;; char-ci=? Tests
(define-test-thunked "char-ci=?-equal-same-case"
  (lambda () (assert-true (char-ci=? #\a #\a))))

(define-test-thunked "char-ci=?-equal-diff-case-ascii"
  (lambda () (assert-true (char-ci=? #\a #\A))))

(define-test-thunked "char-ci=?-unequal-ascii"
  (lambda () (assert-false (char-ci=? #\a #\b))))

(define-test-thunked "char-ci=?-equal-diff-case-unicode"
  (lambda () (assert-true (char-ci=? #\λ #\Λ)))) ; Greek lambda

(define-test-thunked "char-ci=?-unequal-unicode"
  (lambda () (assert-false (char-ci=? #\λ #\μ))))

(define-test-thunked "char-ci=?-non-alpha"
  (lambda () (assert-true (char-ci=? #\1 #\1))))
(define-test-thunked "char-ci=?-non-alpha-unequal"
  (lambda () (assert-false (char-ci=? #\1 #\2))))

;;; char-ci<? Tests
(define-test-thunked "char-ci<?-less-ascii"
  (lambda () (assert-true (char-ci<? #\a #\b))))

(define-test-thunked "char-ci<?-less-diff-case-ascii"
  (lambda () (assert-true (char-ci<? #\A #\b))))
(define-test-thunked "char-ci<?-less-diff-case-ascii-2"
  (lambda () (assert-true (char-ci<? #\a #\B))))

(define-test-thunked "char-ci<?-equal-diff-case-ascii"
  (lambda () (assert-false (char-ci<? #\a #\A))))

(define-test-thunked "char-ci<?-greater-diff-case-ascii"
  (lambda () (assert-false (char-ci<? #\B #\a))))

(define-test-thunked "char-ci<?-less-unicode"
  (lambda () (assert-true (char-ci<? #\α #\Β)))) ; Greek alpha, Beta

;;; char-ci>? Tests
(define-test-thunked "char-ci>?-greater-ascii"
  (lambda () (assert-true (char-ci>? #\b #\a))))

(define-test-thunked "char-ci>?-greater-diff-case-ascii"
  (lambda () (assert-true (char-ci>? #\B #\a))))
(define-test-thunked "char-ci>?-greater-diff-case-ascii-2"
  (lambda () (assert-true (char-ci>? #\b #\A))))

(define-test-thunked "char-ci>?-equal-diff-case-ascii"
  (lambda () (assert-false (char-ci>? #\a #\A))))

(define-test-thunked "char-ci>?-less-diff-case-ascii"
  (lambda () (assert-false (char-ci>? #\A #\b))))

(define-test-thunked "char-ci>?-greater-unicode"
  (lambda () (assert-true (char-ci>? #\Β #\α)))) ; Greek Beta, alpha

;;; char-ci<=? Tests
(define-test-thunked "char-ci<=?-less-ascii"
  (lambda () (assert-true (char-ci<=? #\a #\b))))

(define-test-thunked "char-ci<=?-less-diff-case-ascii"
  (lambda () (assert-true (char-ci<=? #\A #\b))))

(define-test-thunked "char-ci<=?-equal-diff-case-ascii"
  (lambda () (assert-true (char-ci<=? #\a #\A))))

(define-test-thunked "char-ci<=?-greater-diff-case-ascii"
  (lambda () (assert-false (char-ci<=? #\B #\a))))

;;; char-ci>=? Tests
(define-test-thunked "char-ci>=?-greater-ascii"
  (lambda () (assert-true (char-ci>=? #\b #\a))))

(define-test-thunked "char-ci>=?-greater-diff-case-ascii"
  (lambda () (assert-true (char-ci>=? #\B #\a))))

(define-test-thunked "char-ci>=?-equal-diff-case-ascii"
  (lambda () (assert-true (char-ci>=? #\a #\A))))

(define-test-thunked "char-ci>=?-less-diff-case-ascii"
  (lambda () (assert-false (char-ci>=? #\A #\b))))


;;;-----------------------------------------------------------------------------
;;; --- END OF STRING TESTS ---