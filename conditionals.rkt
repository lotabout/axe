#lang racket/base

(require syntax/parse/define)
(require (for-syntax racket/base))
(require racket/match)

(provide if-not
         if-let
         when-not
         when-let
         cond-let)

(module+ test
  (require rackunit)
  (require syntax/macro-testing))

;; -----------------------------------------------------------------------------
;; if-not
(define-simple-macro (if-not (~describe "\"test\" condition" test:expr)
                             (~describe "\"then\" clause" then:expr)
                             (~describe "\"else\" clause" else:expr))
  (if (not test) then else))

(module+ test
  (check-exn exn:fail:syntax?
             (lambda ()
               (convert-compile-time-error
                (if-not))))

  (check-exn exn:fail:syntax?
             (lambda ()
               (convert-compile-time-error
                (if-not #t))))

  (check-exn exn:fail:syntax?
             (lambda ()
               (convert-compile-time-error
                (if-not #t #t))))

  (check-exn exn:fail:syntax?
             (lambda ()
               (convert-compile-time-error
                (if-not #t #t))))

  (check-exn exn:fail:syntax?
             (lambda ()
               (convert-compile-time-error
                (if-not #t #t #t #t))))

  (check-equal? (if-not #t 'true 'false) 'false)
  (check-equal? (if-not #f 'true 'false) 'true))


;; -----------------------------------------------------------------------------
;; if-let
(define-simple-macro (if-let (~describe "binding pairs" [binding:expr value:expr])
                             (~describe "\"then\" clause" then:expr)
                             (~describe "\"else\" clause" else:expr))
  (let ([val value])
    (if val (match-let ([binding val]) then) else)))

(module+ test
  (check-exn exn:fail:syntax?
             (lambda ()
               (convert-compile-time-error
                (if-let))))

  (check-exn exn:fail:syntax?
             (lambda ()
               (convert-compile-time-error
                (if-let []))))

  (check-exn exn:fail:syntax?
             (lambda ()
               (convert-compile-time-error
                (if-let [a]))))

  (check-exn exn:fail:syntax?
             (lambda ()
               (convert-compile-time-error
                (if-let [a #t]))))

  (check-exn exn:fail:syntax?
             (lambda ()
               (convert-compile-time-error
                (if-let [a #t] 'true))))

  (check-exn exn:fail:syntax?
             (lambda ()
               (convert-compile-time-error
                (if-let [a #t] 'true 'false 'extra))))

  (check-equal? (if-let [it '(1 2 3)]
                        it
                        (fail "if-let: wrong-branch"))
                '(1 2 3))

  (check-exn exn:fail:syntax?
             (lambda ()
               (convert-compile-time-error
                (if-let [it #f]
                        (fail "if-let: wrong-branch")
                        it))))

  ;; simple test on match-let
  (check-equal? (if-let [(list a b) '(1 2)]
                        (+ a b)
                        (fail "if-let: wrong-branch"))
                3))

;; -----------------------------------------------------------------------------
;; when-not
(define-simple-macro (when-not (~describe "\"test\" condition" test:expr) body:expr ...+)
  (when (not test) body ...))

(module+ test
  (check-exn exn:fail:syntax?
             (lambda ()
               (convert-compile-time-error
                (when-not))))

  (check-exn exn:fail:syntax?
             (lambda ()
               (convert-compile-time-error
                (when-not #t))))

  (check-equal? (when-not #f 'true) 'true)
  (check-equal? (when-not #t 'true) (void))
  (check-equal? (when-not #f 'one 'two) 'two))



;; -----------------------------------------------------------------------------
;; when-let
(define-simple-macro (when-let (~describe "binding pairs" [binding:expr value:expr]) body:expr ...+)
  (let ([val value])
    (when val (match-let ([binding val]) body ...))))

(module+ test
  (check-exn exn:fail:syntax?
             (lambda ()
               (convert-compile-time-error
                (when-let))))

  (check-exn exn:fail:syntax?
             (lambda ()
               (convert-compile-time-error
                (when-let []))))

  (check-exn exn:fail:syntax?
             (lambda ()
               (convert-compile-time-error
                (when-let [a]))))

  (check-exn exn:fail:syntax?
             (lambda ()
               (convert-compile-time-error
                (when-let [a #t]))))

  (check-equal? (when-let [it '(1 2 3)] it) '(1 2 3))

  (check-equal? (when-let [it '(1 2 3)] it 'last) 'last)

  ;; simple test on match-let
  (check-equal? (when-let [(list a b) '(1 2)] (+ a b)) 3))


;; -----------------------------------------------------------------------------
;; cond-let
(define-syntax (cond-let stx)
  (syntax-parse stx
    #:literals (=> else)
    [(_) #'(void)]
    [(_ it:expr) #'(void)]
    [(_ it:expr () . rest)
     (raise-syntax-error "cond-let"
                         "bad syntax (clause is not a test-value pair)")]

    [(_ it:expr (else . body) . rest )
     (if (not (null? (syntax-e #'rest)))
         (raise-syntax-error "cond-let"
                             "bad syntax (`else' clause must be the last)"
                             #'(else . body ))
         (if (null? (syntax-e #'body))
             #'(void)
             #'(begin . body)))]

    [(_ it:expr (condition:expr => . procs) . rest)
     (let ([len-procs (length (syntax-e #'procs))])
       (if (or (= len-procs 0) (> len-procs 1))
           (raise-syntax-error "cond-let"
                               "bad syntax (bad clause form with =>)"
                               #'(condition => procs))
           (with-syntax ([proc (car (syntax-e #'procs))])
             #'(let ([val condition])
                 (if-let [it val]
                     (proc it)
                     (cond-let it . rest))))))]

    [(_ it:expr (condition:expr . body) . rest)
     (if (null? (syntax-e #'body))
         #'(if-let [it condition]
             it
             (cond-let it . rest))
         #'(if-let [it condition]
             (begin . body)
             (cond-let it . rest)))]))

(module+ test
  ;; syntax error
  (check-exn exn:fail?
             (lambda ()
               (convert-compile-time-error
                (cond-let it ())))
             "invalid test-value should trigger compile error")

  (check-exn exn:fail?
             (lambda ()
               (convert-compile-time-error
                (cond-let it () ())))
             "invalid test-value should trigger compile error")

  (check-exn exn:fail?
             (lambda ()
               (convert-compile-time-error
                (cond-let it (else) ())))
             "non-last \"else\" caluse should trigger error")

  (check-exn exn:fail?
             (lambda ()
               (convert-compile-time-error
                (cond-let it (#t =>))))
             "invalid \"=>\" clause should trigger error")

  (check-exn exn:fail?
             (lambda ()
               (convert-compile-time-error
                (cond-let it (#t => + -))))
             "invalid \"=>\" clause should trigger error")

  (check-exn exn:fail?
             (lambda ()
               (convert-compile-time-error
                (cond-let it (#t => + -) ())))
             "invalid \"=>\" clause should trigger error")

  (check-equal?
   (let ([lst '(x y z a b c)])
     (cond-let it
               [(member 'a lst) it]
               [(member 'b lst) (fail "cond-let: wrong branch")]
               [else (fail "cond-let: wrong branch")]))
   '(a b c))

  (check-equal?
   (let ([lst '(x y z a b c)])
     (cond-let it
               [(member 'not-here lst) (fail "cond-let: wrong branch")]
               [(member 'b lst) it]
               [else (fail "cond-let: wrong branch")]))
   '(b c))

  (check-exn exn:fail:syntax?
   (let ([lst '(x y z a b c)])
     (lambda ()
       (convert-compile-time-error
         (cond-let it
                   [(member 'not-here lst) (fail "cond-let: wrong branch")]
                   [(member 'not-here-too lst) (fail "cond-let: wrong branch")]
                   [else it])))))

  ;; should shadow the outer variable
  (check-equal?
   (let ([lst '(x y z a b c)]
         [it 'outer])
     (cond-let it
               [(member 'a lst) it]
               [(member 'not-here-too lst) (fail "cond-let: wrong branch")]
               [else (fail "cond-let: wrong branch")]))
   '(a b c))

  ;; else clause with no value expression
  (check-equal? (cond-let it [else]) (void))

  ;; in => clause, the variable is also bound
  (check-equal?
   (let ([lst '(x y z a b c)])
     (cond-let it
               [(member 'a lst) => (lambda (val) (equal? val it))]
               [else (fail "cond-let: wrong branch")]))
   #t)


  ;; no branch
  (check-equal? (cond-let) (void))
  (check-equal? (cond-let it) (void))

  ;; single branch that succeeded
  (check-equal?
   (let ([lst '(x y z a b c)])
     (cond-let it
               [(member 'a lst) (length it)]))
   3)

  ;; single branch that failed
  (check-equal?
   (let ([lst '(x y z a b c)])
     (cond-let it
               [(member 'not-here lst) (length it)]))
   (void)))
