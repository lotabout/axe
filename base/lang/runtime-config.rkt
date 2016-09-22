#lang racket/base

(provide configure)

(require (only-in axe/base/axe-reader make-axe-readtable))

(define (configure data)
  (current-readtable (make-axe-readtable)))
