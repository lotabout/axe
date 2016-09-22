#lang racket/base

(require racket/function
         (only-in racket/port input-port-append)
         (only-in axe/escape pregexp-raw regexp-raw))

(provide make-axe-readtable)

(module+ test
  (require rackunit))

(define regex-raw-double-quote #px"\"((?:\\\\.|(?<!\\\\).)*?)(?<!\\\\)\"")
(define regex-raw-single-quote #px"'((?:\\\\.|(?<!\\\\).)*?)(?<!\\\\)'")
(define regex-raw-slash #px"((?:\\\\.|(?<!\\\\).)*?(?<!\\\\))/")

(define (read-regexp-raw src in)
  (define raw-string (bytes->string/locale (cadr (regexp-match regex-raw-slash in))))
  (values (regexp-raw raw-string) (+ 1 (string-length raw-string))))
(define (read-pregexp-raw src in)
  (define raw-string (bytes->string/locale (cadr (regexp-match regex-raw-slash in))))
  (values (pregexp-raw raw-string) (+ 1 (string-length raw-string))))

;;; read-raw-string: (ch src in) -> (data, span)
;;; ch: starting quote
(define (read-raw-string ch src in)
  (define raw-string
    (bytes->string/locale
      (cadr (regexp-match (if (eqv? ch #\") regex-raw-double-quote regex-raw-single-quote) in))))
  (values (parse-escapped-quote raw-string ch) (+ 2 (string-length raw-string))))

;;; In raw string, we use `/` to escape the delimiter, for example r"\"".
;;; That means we should omit the `/` in r"\"".
(define (parse-escapped-quote raw-string quote-ch)
  (define in (open-input-string raw-string))
  (let loop ([ch (read-char in)] [ret '()])
    (if (eof-object? ch)
        (list->string (reverse ret))
        (if (and (eqv? ch #\\)
                 (eqv? (peek-char in) quote-ch))
            (loop (read-char in) ret)
            (loop (read-char in) (cons ch ret))))))

;;; taken from rackjure:
;;; https://github.com/greghendershott/rackjure/blob/master/rackjure/lambda-reader.rkt
(define ((make-reader-proc [orig-readtable (current-readtable)]) ch in src line col pos)
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
           [(peek/read? "x/" in) (wrap-reader read-regexp-raw)]
           [else (unget-normal-read-syntax "r" src in)])]
    [(#\p) ; read #px/
     (cond
       [(peek/read? "x/" in) (wrap-reader read-pregexp-raw)]
       [else (unget-normal-read-syntax "#p" src in)])]
    [else (normal-read-syntax src in)]))

(define (make-axe-readtable [orig-readtable (current-readtable)])
  (define read-proc (make-reader-proc orig-readtable))
  (make-readtable (current-readtable)
                  #\r 'non-terminating-macro read-proc
                  #\r 'dispatch-macro read-proc
                  #\p 'dispatch-macro read-proc))
