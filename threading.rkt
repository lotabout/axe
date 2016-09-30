#lang racket/base

(require racket/stxparam
         syntax/parse/define
         (for-syntax racket/base
                     racket/list
                     syntax/parse))

(provide ~> ~>> and~> and~>> _
         lambda~> lambda~>> lambda~>* lambda~>>*
         lambda-and~> lambda-and~>> lambda-and~>* lambda-and~>>*
         (rename-out [lambda~> λ~>] [lambda~>> λ~>>]
                     [lambda~>* λ~>*] [lambda~>>* λ~>>*]
                     [lambda-and~> λ-and~>] [lambda-and~>> λ-and~>>]
                     [lambda-and~>* λ-and~>*] [lambda-and~>>* λ-and~>>*]))

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

;;; We want to use the #%app unhygenic
;;; This function is used to adjust the context of `stx` to `ctx`
(define-for-syntax (adjust-outer-context ctx stx [srcloc #f])
  (datum->syntax ctx (syntax-e stx) srcloc))

(define-syntax-parser ~>
  [(_ val) #'val]
  [(arrow val func more ...)
   (syntax-case (adjust-outer-context #'arrow (ensure-placeholder #'func)) ()
     [app/ctx
       #'(let ([new-val val])
           (arrow (syntax-parameterize ([_ (make-rename-transformer #'new-val)]) app/ctx)
                  more ...))])])

(define-syntax-parser ~>>
  [(_ val) #'val]
  [(arrow val func more ...)
   (syntax-case (adjust-outer-context #'arrow (ensure-placeholder #'func #:pos 'end)) ()
     [app/ctx
       #'(let ([new-val val])
           (arrow (syntax-parameterize ([_ (make-rename-transformer #'new-val)]) app/ctx)
                  more ...))])])
(define-for-syntax (ensure-placeholder stx #:pos [pos 'front])
    (syntax-parse stx
      #:literals (quote quasiquote)
      [(~or e:id e:keyword) #'(e _)]
      [((~or e:quote e:quasiquote) data ...)
       (if (contains-placeholder? #'(e data ...))
           #'(e data ...)
           #'((e data ...) _))]
      [(e:expr arg ...)
       (if (contains-placeholder? #'(e arg ...))
           #'(e arg ...)
           (if (eq? pos 'front)
               #'(e _ arg ...)
               #'(e arg ... _)))]
      [data #'(data _)]))

(begin-for-syntax


  (define (contains-placeholder? stx)
    (with-handlers ([exn:fail:syntax:placeholder? (lambda (x) #t)]
                    [(lambda (x) #t) (lambda (x) #f)])
      (local-expand (with-syntax ([body stx])
                      #'(syntax-parameterize ([_ default-placeholder])
                                             body))
                    'expression '())
      #f)))

;;; some wrappers over ~> and ~>>

(define-syntax-parser and~>
  [(arrow val func ...)
   (with-syntax
     ([(body ...)
       (datum->syntax #f
                      (map (lambda (stx) (adjust-outer-context #'arrow (ensure-placeholder stx)))
                           (syntax-e #'(func ...)))
                      #f)])
     #'(~> val (and _ body) ...))])

(define-syntax-parser and~>>
  [(arrow val func ...)
   (with-syntax
     ([(body ...)
       (datum->syntax #f
                      (map (lambda (stx) (adjust-outer-context #'arrow (ensure-placeholder stx #:pos 'end)))
                           (syntax-e #'(func ...)))
                      #f)])
     #'(~>> val (and _ body) ...))])

;;; lambda wrappers

(define-syntax-parser lambda~>
  [(arrow . body)
   (with-syntax ([~> (adjust-outer-context #'arrow #'~>)])
     #'(lambda (arg) (~> arg . body)))])

(define-syntax-parser lambda~>>
  [(arrow . body)
   (with-syntax ([~>> (adjust-outer-context #'arrow #'~>>)])
     #'(lambda (arg) (~>> arg . body)))])

(define-syntax-parser lambda~>*
  [(arrow . body)
   (with-syntax ([~> (adjust-outer-context #'arrow #'~>)])
     #'(lambda args (~> args . body)))])

(define-syntax-parser lambda~>>*
  [(arrow . body)
   (with-syntax ([~>> (adjust-outer-context #'arrow #'~>>)])
     #'(lambda args (~>> args . body)))])

(define-syntax-parser lambda-and~>
  [(arrow . body)
   (with-syntax ([and~> (adjust-outer-context #'arrow #'and~>)])
     #'(lambda (arg) (and~> arg . body)))])

(define-syntax-parser lambda-and~>>
  [(arrow . body)
   (with-syntax ([and~>> (adjust-outer-context #'arrow #'and~>>)])
     #'(lambda (arg) (and~>> arg . body)))])

(define-syntax-parser lambda-and~>*
  [(arrow . body)
   (with-syntax ([and~> (adjust-outer-context #'arrow #'and~>)])
     #'(lambda args (and~> args . body)))])

(define-syntax-parser lambda-and~>>*
  [(arrow . body)
   (with-syntax ([and~>> (adjust-outer-context #'arrow #'and~>>)])
     #'(lambda args (and~>> args . body)))])

(module+ test
  (require rackunit)
  (test-case
    "~> and ~>>"
    (check-equal? (~> 10) 10)
    (check-equal? (~> 10 (- 1)) 9)
    (check-equal? (~> 10 (- 1 2)) 7)
    (check-equal? (~> 10 (- 1 2) number->string) "7")
    (check-equal? (~> 10 (- 20 _ 2)) 8)
    (check-equal? (~> 10 (- 20 _ 2 _)) -2)
    (check-equal? (~> 10 (- (* 2 _) _)) 10)
    (check-equal? (~> (* 2 2) (- (* 2 _) _)) 4)
    (check-equal? (~> (* 2 2) (- (* 2 _) _) (- 2)) 2)

    (check-equal? (~>> 10) 10)
    (check-equal? (~>> 10 (- 1)) -9)
    (check-equal? (~>> 10 (- 1 2)) -11)
    (check-equal? (~>> 10 (- 1 2) number->string) "-11")
    (check-equal? (~>> 10 (- 20 _ 2)) 8)
    (check-equal? (~>> 10 (- 20 _ 2 _)) -2)
    (check-equal? (~>> 10 (- (* 2 _) _)) 10)
    (check-equal? (~>> (* 2 2) (- (* 2 _) _)) 4)
    (check-equal? (~>> (* 2 2) (- (* 2 _) _) (- 2)) -2))

  (test-case
    "and~> and ~>>"
    (check-equal? (and~> 'x) 'x)
    (check-equal? (and~>> 'x) 'x)

    (check-equal? (and~> #f string->number) #f)
    (check-equal? (and~>> #f string->number) #f)
    (check-equal? (and~>  '(1 3 5) (findf odd? _) add1) 2)
    (check-equal? (and~>> '(1 3 5) (findf odd?) add1) 2)

    (check-equal? (and~>  '(1 3 5) (findf even? _) add1) #f)
    (check-equal? (and~>> '(1 3 5) (findf even?) add1) #f))

  (test-case
    "unhygienic #%app test"

    (check-equal? (let-syntax ([#%app (syntax-rules () [(_ . rest) (list . rest)])])
                    (~> 1 (2) (3)))
                  '(3 (2 1)))
    (check-equal? (let-syntax ([#%app (syntax-rules () [(_ . rest) (list . rest)])])
                    (~>> 1 (2) (3)))
                  '(3 (2 1)))
    (check-equal? (let-syntax ([#%app (syntax-rules () [(_ . rest) (list . rest)])])
                    (and~> 1 (2) (3)))
                  '(3 (2 1)))
    (check-equal? (let-syntax ([#%app (syntax-rules () [(_ . rest) (list . rest)])])
                    (and~>> 1 (2) (3)))
                  '(3 (2 1)))

    (check-equal? (let-syntax ( [#%app (syntax-rules () [(_ . rest) (list . rest)])])
                    (#%plain-app (lambda~> (2) (3)) 1))
                  '(3 (2 1)))
    (check-equal? (let-syntax ([#%app (syntax-rules () [(_ . rest) (list . rest)])])
                    (#%plain-app (lambda~>> (2) (3)) 1))
                  '(3 (2 1)))
    (check-equal? (let-syntax ([#%app (syntax-rules () [(_ . rest) (list . rest)])])
                    (#%plain-app (lambda-and~> (2) (3)) 1))
                  '(3 (2 1)))
    (check-equal? (let-syntax ([#%app (syntax-rules () [(_ . rest) (list . rest)])])
                    (#%plain-app (lambda-and~>> (2) (3)) 1))
                  '(3 (2 1)))))
