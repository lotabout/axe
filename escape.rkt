#lang racket/base

(provide regex-escape-raw
         pregexp-raw
         regexp-raw)

(module+ test
  (require rackunit))

;;; wrap-escape: helper function to escape backslash if "\\<char>" can be recognized by racket
;;; reader
;;;
;;; "\\d\\t" -> "\"\\\\d\\t\""
;;;
;;; 1. "\\d" cannot be parsed by racket reader, thus escape its '\' characters, into "\\\\d"
;;; 2. "\\t" can be parsed by racket reader, keep it as original: "\\t"
;;; 3. wrap the whole result with quotes, resulting: "\"\\\\d\\t\""
;;; It will later be passed for `read`
(define (wrap-escape str)
  (define in (open-input-string str))
  (let loop ([ch (read-char in)] [ret '(#\")])
    (if (eof-object? ch)
        (list->string (reverse (cons #\" ret)))
        (cond
          [(eq? ch #\\)
           (define next (peek-char in))
           (cond
             [(eqv? next #\\) ; consume '\\'
              (read-char in)
              (loop (read-char in) (cons #\\ (cons #\\ (cons #\\ (cons #\\ ret)))))]
             [(ormap (lambda (ch) (eqv? ch next))
                      (list #\a #\b #\t #\n #\v #\f #\r #\e #\x #\u #\U #\newline #\linefeed
                            #\return))
              ; leave it as it is
              (loop (read-char in) (cons #\\ ret))]
             [else
               ; escape character '\'
               (loop (read-char in) (cons #\\ (cons #\\ ret)))])]
          [(eqv? ch #\")
           (loop (read-char in) (cons #\" (cons #\\ ret)))]
          [else (loop (read-char in) (cons ch ret))]))))

(module+ test
  (check-equal? (wrap-escape "\\a\\b\\t\\n\\v\\f\\r\\e\\x\\u\\U")
                "\"\\a\\b\\t\\n\\v\\f\\r\\e\\x\\u\\U\"")
  (check-equal? (wrap-escape "escaped quotes: \\\"\\'")
                "\"escaped quotes: \\\\\\\"\\\\'\"")
  (check-equal? (wrap-escape "\\d\\1") "\"\\\\d\\\\1\"")
  (check-equal? (wrap-escape "double quote: \"") "\"double quote: \\\"\"")
  (check-equal? (wrap-escape "escaped: \\\\") "\"escaped: \\\\\\\\\""))

;;; escape the backslash('\') in a string for regexp
;;;
;;; escape will parse the escapped character in a raw string in the same way the racket
;;; reader handles strings
;;;
;;; "\\d\\t" -> "\\d\t"
;;; because "\\t" can be read by racket reader into string "\t", however "\\d" is not recognized.
(define (regex-escape-raw str)
  (read (open-input-string (wrap-escape str))))

(module+ test
  (check-equal? (regex-escape-raw "\\a\\b\\t\\n\\v\\f\\r\\e\\x64\\u4e2d\\U4e2d")
                "\a\b\t\n\v\f\r\e\x64\u4e2d\U4e2d")
  (check-equal? (regex-escape-raw "escaped quotes: \\\"\\'")
                "escaped quotes: \\\"\\'")
  (check-equal? (regex-escape-raw "\\d\\1") "\\d\\1")
  (check-equal? (regex-escape-raw "double quote: \"") "double quote: \""))

;;; wrappers over raw string
(define (pregexp-raw str) (pregexp (regex-escape-raw str)))
(define (regexp-raw str) (regexp (regex-escape-raw str)))
