#lang racket

(require racket/stxparam
         (for-syntax racket/base
                     racket/list
                     syntax/parse))

;;; An implementation for threading macro which is clojure like.
;;; Thanks to http://jeapostrophe.github.io/2013-05-27-stxparam-post.html
;;; Also check out: http://docs.racket-lang.org/threading/index.html

;;; ~> or ~>> is used to chain operations together.
;;;
;;; For example, you might want to write:
;;; (- (bytes-ref (string->bytes/utf-8 (symbol->string 'abc)) 1) 2)
;;;
;;; With threading macro, you can use steam processing thinking
;;; (~> 'abc
;;;     symbol->string
;;;     string->bytes/utf-8
;;;     (bytes-ref 1)
;;;     (- 2))
;;;
;;; That is you prepare the initial glossary 'abc and pass it to symbol->string
;;; and pass the resulting data to string->bytes/utf-8, and ...
;;;
;;; Note that with ~>, it will put the result from previous function as the
;;; *first* argument of the next function.
;;;
;;; (~> 10 (- 2)) becomes (- 10 2).
;;;
;;; And ~>> will put it as the *last* argument of the next function
;;;
;;; (~> 10 (- 2)) becomes (- 2 10)
;;;
;;; If you want to use the result in the middle of the calling: use `_` as a placeholder:
;;;
;;; (~> 10 (- (* 2 _ 3) _ 20)) => (- (* 2 10 3) 10 20)

(begin-for-syntax
  (struct exn:fail:syntax:placeholder exn:fail:syntax ())
  (define default-placeholder
    (lambda (stx)
      (raise (exn:fail:syntax:placeholder
               "_: Only allowed inside ~>"
               (current-continuation-marks)
               (list stx))))))

(define-syntax-parameter _ default-placeholder)

(define-syntax ~>
  (syntax-rules ()
    [(_ val)
     val]
    [(_ val func more ...)
     (let ([new-val val])
       (~> (syntax-parameterize ([_ (make-rename-transformer #'new-val)])
                                (ensure-placeholder/front func))
           more ...))]))
(define-syntax ~>>
  (syntax-rules ()
    [(_ val)
     val]
    [(_ val func more ...)
     (let ([new-val val])
       (~> (syntax-parameterize ([_ (make-rename-transformer #'new-val)])
                                (ensure-placeholder/end func))
           more ...))]))

(define-syntax (ensure-placeholder/front stx)
  (syntax-parse stx
    [(_ e:id)
     #'(e _)]
    [(_ (e:expr arg ...))
     (if (contains-placeholder? #'(e arg ...))
         #'(e arg ...)
         #'(e _ arg ...))]))

(define-syntax (ensure-placeholder/end stx)
  (syntax-parse stx
    [(_ e:id)
     #'(e _)]
    [(_ (e:expr arg ...))
     (if (contains-placeholder? #'(e arg ...))
         #'(e arg ...)
         #'(e arg ... _))]))

(begin-for-syntax
  (define (contains-placeholder? stx)
    (with-handlers ([exn:fail:syntax:placeholder? (lambda (x) #t)]
                    [(lambda (x) #t) (lambda (x) #f)])
      (local-expand (with-syntax ([body stx])
                      #'(syntax-parameterize ([_ default-placeholder])
                                             body))
                    'expression '())
      #f)))

(module+ test
  (require rackunit)
  (check-equal? (~> 10) 10)
  (check-equal? (~> 10 (- 1)) 9)
  (check-equal? (~> 10 (- 1 2)) 7)
  (check-equal? (~> 10 (- 1 2) number->string) "7")
  (check-equal? (~> 10 (- 20 _ 2)) 8)
  (check-equal? (~> 10 (- 20 _ 2 _)) -2)
  (check-equal? (~> 10 (- (* 2 _) _)) 10)
  (check-equal? (~> (* 2 2) (- (* 2 _) _)) 4)

  (check-equal? (~>> 10) 10)
  (check-equal? (~>> 10 (- 1)) -9)
  (check-equal? (~>> 10 (- 1 2)) -11)
  (check-equal? (~>> 10 (- 1 2) number->string) "-11")
  (check-equal? (~>> 10 (- 20 _ 2)) 8)
  (check-equal? (~>> 10 (- 20 _ 2 _)) -2)
  (check-equal? (~>> 10 (- (* 2 _) _)) 10)
  (check-equal? (~>> (* 2 2) (- (* 2 _) _)) 4))

(provide ~> ~>> _)
