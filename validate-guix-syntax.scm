#!/usr/bin/guile -s
!#

;;; Syntax validation script for guix.scm
;;; This validates the Scheme syntax without requiring full Guix installation

(use-modules (ice-9 pretty-print)
             (ice-9 match))

(define (validate-file filename)
  "Validate the syntax of a Scheme file by reading all expressions."
  (format #t "Validating ~a...~%" filename)
  
  (catch #t
    (lambda ()
      (call-with-input-file filename
        (lambda (port)
          (let loop ((expr (read port))
                     (count 0))
            (if (eof-object? expr)
                (begin
                  (format #t "✓ Successfully read ~a expressions~%" count)
                  (format #t "✓ Syntax validation PASSED~%")
                  #t)
                (begin
                  (when (= (modulo count 10) 0)
                    (format #t "  Read ~a expressions...~%" count))
                  (loop (read port) (+ count 1))))))))
    (lambda (key . args)
      (format #t "✗ Syntax validation FAILED~%")
      (format #t "Error: ~a~%" key)
      (format #t "Details: ~a~%" args)
      #f)))

(define (check-balanced-parens filename)
  "Check if parentheses are balanced in the file."
  (format #t "~%Checking parenthesis balance...~%")
  
  (call-with-input-file filename
    (lambda (port)
      (let loop ((char (read-char port))
                 (depth 0)
                 (max-depth 0)
                 (line 1)
                 (col 0))
        (cond
         ((eof-object? char)
          (if (= depth 0)
              (begin
                (format #t "✓ Parentheses are balanced~%")
                (format #t "  Maximum nesting depth: ~a~%" max-depth)
                #t)
              (begin
                (format #t "✗ Unbalanced parentheses: ~a unclosed~%" depth)
                #f)))
         ((eq? char #\()
          (loop (read-char port) (+ depth 1) (max (+ depth 1) max-depth) line (+ col 1)))
         ((eq? char #\))
          (if (< depth 1)
              (begin
                (format #t "✗ Extra closing parenthesis at line ~a, column ~a~%" line col)
                #f)
              (loop (read-char port) (- depth 1) max-depth line (+ col 1))))
         ((eq? char #\newline)
          (loop (read-char port) depth max-depth (+ line 1) 0))
         (else
          (loop (read-char port) depth max-depth line (+ col 1))))))))

(define (analyze-structure filename)
  "Analyze the structure of the guix.scm file."
  (format #t "~%Analyzing package structure...~%")
  
  (catch #t
    (lambda ()
      (call-with-input-file filename
        (lambda (port)
          (let* ((expr (read port))
                 (uses (if (and (pair? expr) (eq? (car expr) 'use-modules))
                           expr
                           #f))
                 (pkg-expr (if uses (read port) expr)))
            
            (when uses
              (format #t "✓ Found use-modules declaration~%")
              (format #t "  Imported ~a modules~%" (- (length uses) 1)))
            
            (match pkg-expr
              (('define-public name body ...)
               (format #t "✓ Found define-public for: ~a~%" name)
               (format #t "✓ Package structure appears valid~%")
               #t)
              (_
               (format #t "⚠ Unexpected top-level form~%")
               #t))))))
    (lambda (key . args)
      (format #t "⚠ Could not analyze structure: ~a~%" key)
      #t)))

;; Main validation
(define (main args)
  (if (< (length args) 2)
      (begin
        (format #t "Usage: ~a <guix.scm>~%" (car args))
        (exit 1))
      (let ((filename (cadr args)))
        (format #t "~%=== Guix Package Syntax Validator ===~%~%")
        
        (let ((result1 (check-balanced-parens filename))
              (result2 (validate-file filename))
              (result3 (analyze-structure filename)))
          
          (format #t "~%=== Validation Summary ===~%")
          (if (and result1 result2)
              (begin
                (format #t "✓ All syntax checks PASSED~%")
                (format #t "~%The guix.scm file is syntactically valid.~%")
                (exit 0))
              (begin
                (format #t "✗ Some checks FAILED~%")
                (format #t "~%Please fix the errors above.~%")
                (exit 1)))))))

(main (command-line))
