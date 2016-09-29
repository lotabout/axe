(module reader racket/base
  (require syntax/module-reader (only-in "../axe-reader.rkt" axe-wrapper))

  (provide (rename-out [axe-read read]
                       [axe-read-syntax read-syntax]
                       [axe-get-info get-info]))

  (define-values (axe-read axe-read-syntax axe-get-info)
    (make-meta-reader
      'axe/base
      "language path"
      (lambda (bstr)
        (let* ([str (bytes->string/latin-1 bstr)]
               [sym (string->symbol str)])
          (and (module-path? sym)
               (vector
                 ;; try submod first:
                 `(submod ,sym reader)
                 ;; fall back to /lang/reader:
                 (string->symbol (string-append str "/lang/reader"))))))
      axe-wrapper
      (lambda (orig-read-syntax)
        (define read-syntax (axe-wrapper orig-read-syntax))
        (lambda args
          (define stx (apply read-syntax args))
          (define old-prop (syntax-property stx 'module-language))
          (define new-prop `#(axe/base/lang/language-info get-language-info #f))
          (syntax-property stx 'module-language new-prop)))
      (lambda (proc)
        (lambda (key defval)
          (define (fallback) (if proc (proc key defval) defval))
          (define (try-dynamic-require mod export)
            (or (with-handlers ([exn:fail? (lambda (x) #f)])
                  (dynamic-require mod export))
                (fallback)))
          (case key
            [(color-lexer)
             (try-dynamic-require 'syntax-color/scribble-lexer 'scribble-lexer)]
            [(definitions-text-surrogate)
             'scribble/private/indentation]
            [else (fallback)]))))))

;#lang s-exp syntax/module-reader
;axe/base
;#:wrapper1 axe-wrapper
;#:language-info #(axe/base/lang/language-info get-language-info #f)

;(require "../axe-reader.rkt")
