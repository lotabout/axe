#lang racket
(provide read read-syntax get-info)

(require (prefix-in - "reader-no-wrap.rkt")
         "../base/axe-reader.rkt")

(define (read in p ln col pos) (axe-wrapper (lambda () (-read in p ln col pos))))
(define (read-syntax src in p ln col pos) (axe-wrapper (λ () (-read-syntax src in p ln col pos))))
(define get-info -get-info)
