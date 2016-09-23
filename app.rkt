#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     syntax/parse))

(provide -#%app
         current-curly-dict)

;;; Expand `{k v ... ...}` as `((current-curly-dict) k v ... ...)`.
;;; current-curly-dict can be `hash`, `hasheq`, `alist` or similar

(define current-curly-dict (make-parameter hash))

(define-syntax (-#%app stx)
  (syntax-parse stx
    ;; {key val ...} dict literals
    [(_ x:expr ...) #:when (eq? (syntax-property stx 'paren-shape) #\{)
      (define key-vals (syntax->list #'(x ...)))
      (unless (zero? (remainder (length key-vals) 2))
        (raise-syntax-error
          '|{ }|
          "missing value for key. Key and value should appear in pairs"
          #'(x ...)
          (last key-vals)))
      #'((current-curly-dict) x ...)]
    ;; else, fallback to racket's #%app
    [(_  f a ...) #'(#%app f a ...)]))
