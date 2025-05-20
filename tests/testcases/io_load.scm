;;; Tests for Basic I/O and Load

;; Note: Testing actual output of display/newline is hard here.
;; We just check the return value (should be unspecified -> NIL).

(define-test-thunked "display-return-value" (lambda () (assert-equal (display "hello") '())))
(define-test-thunked "newline-return-value" (lambda () (assert-equal (newline) '())))

;; Test loading another file
;; Create a simple file to be loaded first
;; (This assumes the test runner runs from a directory where it can create files,
;; or that this file exists relative to the test execution path)

;; Create a dummy file to load (e.g., load_target.scm)
;; Content of load_target.scm:
;;   (define loaded-variable 999)

;; If file creation isn't feasible in the test environment,
;; manually create tests/testcases/load_target.scm with:
;; (define loaded-variable 999)

;; Assuming tests/testcases/load_target.scm exists:
(define-test-thunked "load-file-check-var"
  (lambda ()
    (begin
      (load "../tests/testcases/data/load_target.scm") ; Path relative to execution dir
      (assert-equal loaded-variable 999))))