#lang racket/base

(provide configure)

(require (only-in axe/base/regex-reader make-/-readtable))

(define (configure data)
  (current-readtable (make-/-readtable)))
