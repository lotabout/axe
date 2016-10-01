#lang racket/base

(provide configure)

(require (only-in axe/base/axe-reader make-axe-readtable current-syntax-introducer))

(define (configure data)
  (current-syntax-introducer (make-syntax-introducer))
  (current-readtable (make-axe-readtable)))
