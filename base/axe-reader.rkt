#lang racket/base

(require (for-syntax racket/base
                     racket/list)
         racket/function
         racket/set
         racket/list
         racket/match
         syntax/parse
         (only-in racket/port input-port-append)
         (only-in axe/escape pregexp-raw regexp-raw regex-escape-raw)
         (only-in version/utils version<=?)
         (for-meta -10 racket/base racket/set)
         (for-meta -9 racket/base racket/set)
         (for-meta -8 racket/base racket/set)
         (for-meta -7 racket/base racket/set)
         (for-meta -6 racket/base racket/set)
         (for-meta -5 racket/base racket/set)
         (for-meta -4 racket/base racket/set)
         (for-meta -3 racket/base racket/set)
         (for-meta -2 racket/base racket/set)
         (for-meta -1 racket/base racket/set)
         (for-meta 0 racket/base racket/set)
         (for-meta 1 racket/base racket/set)
         (for-meta 2 racket/base racket/set)
         (for-meta 3 racket/base racket/set)
         (for-meta 4 racket/base racket/set)
         (for-meta 5 racket/base racket/set)
         (for-meta 6 racket/base racket/set)
         (for-meta 7 racket/base racket/set)
         (for-meta 8 racket/base racket/set)
         (for-meta 9 racket/base racket/set)
         (for-meta 10 racket/base racket/set)
         (for-meta 11 (only-in racket/base #%app make-rename-transformer syntax))
         )

(provide make-axe-readtable
         axe-wrapper
         current-syntax-introducer)

(module+ test
  (require rackunit))

(define current-syntax-introducer
  (make-parameter
    (lambda (stx)
      (error 'current-syntax-introducer "must be used within the axe reader"))))

;;;=============================================================================
;;; Raw strings, regexps

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

;;;=============================================================================
;;; Literal lambda
;;; Based on curly-fn: https://github.com/lexi-lambda/racket-curly-fn

; struct to store argument information of a lambda literal: #(a b %)
(struct argument-info (max-positional has-rest? keywords) #:transparent)
(define empty-argument-info (argument-info 0 #f '()))

; merge two argument-info structure
(define/match (merge-argument-info a b)
  [((argument-info m r k)
    (argument-info mm rr kk))
   (argument-info (max m mm)
                  (or r rr)
                  (remove-duplicates (append k kk)))])

; merge a list of argument-info
(define (merge-list-of-argument-info . args)
  (foldl merge-argument-info empty-argument-info args))
(define (merge-list-of-argument-info* args)
  (apply merge-list-of-argument-info args))

; parse the usage of %, %n, %& and %:kw in a syntax object
(define (parse-arguments stx)
  (syntax-parse stx
    #:datum-literals (quote)
    ; examine identifiers to see if they are special
    [id:id (parse-arguments/symbol (syntax-e #'id))]
    ; stop on literals
    [((~datum quote) . _) empty-argument-info]
    ; recursively search through lists
    [(form ...) (merge-list-of-argument-info* (map parse-arguments (syntax->list stx)))]
    ; otherwise, return null
    [_ empty-argument-info]))

; identifies the type of argument represented by a symbol
(define (parse-arguments/symbol sym)
  (match (symbol->string sym)
    ["%" (argument-info 1 #f '())]
    ["%&" (argument-info 0 #t '())]
    [(regexp #px"%(\\d+)" (list _ (app string->number n)))
     (argument-info n #f null)]
    [(regexp #px"%:(.+)" (list _ (app string->keyword k)))
     (argument-info 0 #f (list k))]
    [_ empty-argument-info]))

(define (stx->args stx)
  (match-let ([(argument-info max-positional has-rest? keywords)
               (parse-arguments stx)])
    (define datum-kw-formals
      (append (for/list ([n (in-range 1 (add1 max-positional))])
                (string->symbol (string-append "%" (number->string n))))
              (append*
                (for/list ([kw (in-list keywords)])
                  (list kw (string->symbol (string-append "%:" (keyword->string kw))))))
              (if has-rest? '%& '())))
    (datum->syntax stx datum-kw-formals stx)))

(define (parse-lambda-literal stx)
  (define intro (current-syntax-introducer))
  (define stx* (intro stx))
  (with-syntax ([args (stx->args stx*)]
                [% (datum->syntax stx* '% stx)]
                [%1 (datum->syntax stx* '%1 stx)]
                [body stx*])
    (intro
      #'(lambda args
          (define-syntax % (make-rename-transformer #'%1))
          body))))

;;;=============================================================================
;;; Build read tables

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
     (define intro (current-syntax-introducer))
     (define stx* (intro (unget-normal-read-syntax "#{" src in)))
     (syntax-case stx* ()
       [#(e ...) (intro #'(set e ...))])]
    [(#\() ; lambda literal #( ... )
     (parse-lambda-literal (unget-normal-read-syntax "(" src in))]
    [(#\f) ; lambda literal #fn( ... )
     (cond [(peek/read? "n(" in)
            (parse-lambda-literal (unget-normal-read-syntax "(" src in))]
           [else (unget-normal-read-syntax "#f" src in)])]
    [(#\l) ; lambda literal #fn( ... )
     (cond [(peek/read? "ambda(" in)
            (parse-lambda-literal (unget-normal-read-syntax "(" src in))]
           [else (unget-normal-read-syntax "#l" src in)])]
    [(#\λ) ; lambda literal #λ( ... )
     (parse-lambda-literal (normal-read-syntax src in))]
    [else (normal-read-syntax src in)]))

(define (make-axe-readtable [orig-readtable (current-readtable)])
  (define read-proc (make-reader-proc orig-readtable))
  (make-readtable (current-readtable)
                  #\r 'non-terminating-macro (curry read-proc #f)
                  #\: 'non-terminating-macro (curry read-proc #f)
                  #\, 'terminating-macro (curry read-proc #f)
                  #\r 'dispatch-macro (curry read-proc #t)
                  #\p 'dispatch-macro (curry read-proc #t)
                  #\{ 'dispatch-macro (curry read-proc #t)
                  #\( 'dispatch-macro (curry read-proc #t)
                  #\f 'dispatch-macro (curry read-proc #t)
                  #\l 'dispatch-macro (curry read-proc #t)
                  #\λ 'dispatch-macro (curry read-proc #t)))

;; A `#:wrapper1` for `syntax/module-reader`
(define (axe-wrapper thk)
  (define orig-readtable (current-readtable))
  (define intro (make-syntax-introducer))
  (parameterize ([current-readtable (make-axe-readtable orig-readtable)]
                 [current-syntax-introducer intro])
    (define stx (thk))
    (if (and (syntax? stx) (version<=? "6.2.900.4" (version)))
        (intro stx)
        stx)))
