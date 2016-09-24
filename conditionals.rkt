#lang racket

(require syntax/parse/define)

(provide if-not
         if-let
         when-not
         when-let)

(define-simple-macro (if-not test:expr then:expr else:expr)
  (if (not test) then else))

(define-simple-macro (if-let [binding:expr value:expr] then:expr else:expr)
  (let ([val value])
    (if val (match-let ([binding val]) then) else)))

(define-simple-macro (when-not test:expr body:expr ...+)
  (when (not test) body ...))

(define-simple-macro (when-let [binding:expr value:expr] body:expr ...+)
  (let ([val value])
    (when val (match-let ([binding val])  body ...))))
