#lang racket/base

(require racket/function
         racket/set
         (only-in racket/port input-port-append)
         (only-in axe/escape pregexp-raw regexp-raw regex-escape-raw))

(provide make-axe-readtable)

(module+ test
  (require rackunit))

(define regex-raw-double-quote #px"\"((?:\\\\.|(?<!\\\\).|\\\\\\\\)*?)(?:(?<!\\\\)\"|(?<=\\\\\\\\)\")")
(define regex-raw-single-quote #px"'((?:\\\\.|(?<!\\\\).|\\\\\\\\)*?)(?:(?<!\\\\)'|(?<=\\\\\\\\)')")
(define regex-raw-slash #px"((?:\\\\.|(?<!\\\\).|\\\\\\\\)*?)(?:(?<!\\\\)/|(?<=\\\\\\\\)/)")

(define (read-regexp-str-raw src in)
  (define raw-string (bytes->string/locale (cadr (regexp-match regex-raw-slash in))))
  (values (regex-escape-raw (restore-escaped-char raw-string #\/)) (+ 1 (string-length raw-string))))

(define (read-regexp-raw src in)
  (define raw-string (bytes->string/locale (cadr (regexp-match regex-raw-slash in))))
  (values (regexp-raw (restore-escaped-char raw-string #\/)) (+ 1 (string-length raw-string))))

(define (read-pregexp-raw src in)
  (define raw-string (bytes->string/locale (cadr (regexp-match regex-raw-slash in))))
  (values (pregexp-raw (restore-escaped-char raw-string #\/)) (+ 1 (string-length raw-string))))

;;; read-raw-string: (quote-char src in) -> (data, span)
(define (read-raw-string quote-char src in)
  (define raw-string
    (bytes->string/locale
      (cadr (regexp-match
              (if (eqv? quote-char #\") regex-raw-double-quote regex-raw-single-quote) in))))
  (values (restore-escaped-char raw-string quote-char) (+ 2 (string-length raw-string))))

;;; In raw string, we use `\` to escape the delimiter, for example r"\"".
;;; That means we should omit the `\` in r"\"".
(define (restore-escaped-char raw-string escaped-char)
  (define in (open-input-string raw-string))
  (let loop ([ch (read-char in)] [ret '()])
    (if (eof-object? ch)
        (list->string (reverse ret))
        (if (and (eqv? ch #\\)
                 (eqv? (peek-char in) escaped-char))
            (loop (read-char in) ret)
            (loop (read-char in) (cons ch ret))))))

;;; s
(define-syntax-rule (syntax/list->set e ...)
  (set e ...))

;;; taken from rackjure:
;;; https://github.com/greghendershott/rackjure/blob/master/rackjure/lambda-reader.rkt
(define ((make-reader-proc [orig-readtable (current-readtable)]) dispatch? ch in src line col pos)
  (define (normal-read-syntax src in)
    (parameterize ([current-readtable orig-readtable])
      (read-syntax src in)))
  (define (unget-normal-read-syntax str src in)
    (normal-read-syntax src (input-port-append #f (open-input-string str) in)))
  (define (peek/read? str in)
    (and (equal? str (peek-string (string-length str) 0 in))
         (read-string (string-length str) in)))

  (define (wrap-reader reader)
    (let-values ([(data span) (reader src in)])
      (datum->syntax #f data (vector src line col pos span))))

  ; real logic for handling characters
  (case ch
    [(#\r)  ; read raw string
     (cond [(or (eqv? (peek-char in) #\') (eqv? (peek-char in) #\"))
            (wrap-reader (curry read-raw-string (peek-char in)))]
           [(peek/read? "/" in) (wrap-reader read-regexp-str-raw)]
           [(peek/read? "x/" in) (wrap-reader read-regexp-raw)]
           [else (unget-normal-read-syntax (if dispatch? "#r" "r") src in)])]
    [(#\:) (unget-normal-read-syntax "#:" src in)]
    [(#\p) ; read #px/
     (cond
       [(peek/read? "x/" in) (wrap-reader read-pregexp-raw)]
       [else (unget-normal-read-syntax "#p" src in)])]
    [(#\,) ; use `,` as space
     (if (char-whitespace? (peek-char in))
         (normal-read-syntax src in)
         (unget-normal-read-syntax "," src in))]
    [(#\{) ; #{1 2 3 4} to read hash set (set 1 2 3 4)
     (syntax-case (unget-normal-read-syntax "#{" src in) ()
       [#(e ...) #'(set e ...)])]
    [else (normal-read-syntax src in)]))

(define (make-axe-readtable [orig-readtable (current-readtable)])
  (define read-proc (make-reader-proc orig-readtable))
  (make-readtable (current-readtable)
                  #\r 'non-terminating-macro (curry read-proc #f)
                  #\: 'non-terminating-macro (curry read-proc #f)
                  #\, 'terminating-macro (curry read-proc #f)
                  #\r 'dispatch-macro (curry read-proc #t)
                  #\p 'dispatch-macro (curry read-proc #t)
                  #\{ 'dispatch-macro (curry read-proc #t)))
