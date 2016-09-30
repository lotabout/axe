#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     syntax/parse)
         racket/dict)

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
    ;; applicable dict (dict key) or (key dict), default to #f
    [(_ x_:expr y_:expr)
     #'(let ([x x_] [y y_])
         (cond [(procedure? x) (#%app x y)]
               [(dict? x) (dict-ref x y #f)] ; (dict key)
               [(not (dict? y)) #f]          ; (key #f) => #f
               [(dict? y) (dict-ref y x #f)] ; (key dict)
               [else (error 'applicable-dict "Dict should be specified in either (~v ~v)" x y)]))]

    [(_ x_:expr y_:expr d_:expr)
     #'(let ([x x_] [y y_] [d d_])
         (cond [(procedure? x) (#%app x y d)]
               [(dict? x) (dict-ref x y d)] ; (dict key default)
               [(dict? y) (dict-ref y x d)] ; (key dict default)
               [else (error 'applicable-dict "Dict should be specified in either (~v ~v)" x y)]))]

    ;; else, fallback to racket's #%app
    [(_  f a ...) #'(#%app f a ...)]))
